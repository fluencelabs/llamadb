use byteutils;
use databaseinfo::{ColumnInfo, TableInfo};
use identifier::Identifier;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt;
use types::DbType;

pub enum UpdateError {
    ValidationError { column_name: Identifier },
}

impl fmt::Display for UpdateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &UpdateError::ValidationError { ref column_name } => {
                write!(f, "Problem validating column: {}", column_name)
            }
        }
    }
}

/// Table representation.
#[derive(Debug)]
pub struct Table {
    /// Name of this table
    pub name: Identifier,

    /// Sequence of all column in this table
    pub columns: Vec<Column>,

    /// Id for next row, increase when new row inserted
    pub next_rowid: u64,

    /// Set of rows for this table in the binary form.
    /// | row_id: 8 bytes | is_null: 1 byte | column_1: 8b, ..., column_N: 8b |
    /// | variable_len_1: 8b, ..., variable_len_N: 8b|
    pub rows_set: BTreeSet<Vec<u8>>,
}

/// Column representation.
#[derive(Debug)]
pub struct Column {
    /// Serial number of this column in its table
    pub offset: u32,

    /// Name of this column
    pub name: Identifier,

    /// Type of this column
    pub dbtype: DbType,

    /// If 'true' nulls are allowed in this column
    pub nullable: bool,
}

impl TableInfo for Table {
    type Column = Column;

    fn get_name(&self) -> &Identifier {
        &self.name
    }

    fn get_column_count(&self) -> u32 {
        self.columns.len() as u32
    }

    fn find_column_by_offset(&self, offset: u32) -> Option<&Column> {
        let i = offset as usize;

        if i < self.columns.len() {
            Some(&self.columns[i])
        } else {
            None
        }
    }

    fn find_column_by_name(&self, name: &Identifier) -> Option<&Column> {
        self.columns.iter().find(|c| &c.name == name)
    }
}

impl Table {
    /// Reads data (column records/values) for specified row.
    pub fn read_data(&self, raw_key: &[u8]) -> Vec<(Box<[u8]>, Option<bool>)> {
        debug!("read_data starts with row={:?}", raw_key);
        let data_len = self.read_data_length(&raw_key);

        debug!("data section length is: {:?}", data_len);

        let mut variable_length_offset = 0;
        let mut key_offset = 8;

        let result = self
            .columns
            .iter()
            .map(|column| {
                let is_null = if column.nullable {
                    let flag = raw_key[key_offset];
                    key_offset += 1;
                    Some(flag != 0)
                } else {
                    None // if column isn't nullable returns None
                };

                let size = match column.dbtype.get_fixed_length() {
                    Some(l) => l as usize,
                    None => {
                        let l = data_len[variable_length_offset];
                        variable_length_offset += 1;
                        l as usize
                    }
                };

                let bytes = raw_key[key_offset..key_offset + size]
                    .to_vec()
                    .into_boxed_slice();

                debug!("record read: {:?}, {:?}", column.dbtype, bytes);
                key_offset += size;

                (bytes, is_null)
            })
            .collect();

        result
    }

    /// Returns length of data section in specified row.
    pub fn read_data_length(&self, row: &[u8]) -> Vec<u64> {
        let variable_column_count = self
            .columns
            .iter()
            .filter(|column| column.dbtype.is_variable_length())
            .count();

        (0..variable_column_count)
            .map(|i| {
                let o = row.len() - variable_column_count * 8 + i * 8;
                byteutils::read_udbinteger(&row[o..o + 8])
            })
            .collect()
    }

    /// Updates row with new values.
    pub fn update_row(
        &mut self,
        old_row: &[u8],
        new_val: &HashMap<usize, (Box<[u8]>, Option<bool>)>,
    ) -> Result<bool, UpdateError> {
        // take only first 8 bytes with row id from old row
        let mut new_row: Vec<u8> = old_row[..8].to_vec();
        debug!("ID is {:?}", &new_row);

        let new_row_data =
            self.read_data(old_row)
                .into_iter()
                .enumerate()
                .map(|(idx, old_record)| {
                    // if the column for this idx has new value returns it,
                    // returns old value otherwise
                    new_val.get(&idx).unwrap_or(&old_record).clone()
                });

        debug!(
            "new_row_data is {:?}",
            new_row_data.clone().collect::<Vec<_>>()
        );

        self.write_data_to_row(new_row_data, &mut new_row)?;

        debug!(
            "Updating row with old_val={:?}, new_val={:?}",
            &old_row, &new_row
        );

        self.delete_row(old_row)
            .and_then(move |_| self.insert_raw_row(new_row))?;

        Ok(true)
    }

    /// rowid is automatically added, and is not included as a specified column
    pub fn insert_new_row<I>(&mut self, column_data: I) -> Result<(), UpdateError>
    where
        I: ExactSizeIterator,
        I: Iterator<Item = (Box<[u8]>, Option<bool>)>,
    {
        assert_eq!(self.columns.len(), column_data.len());

        let mut new_row: Vec<u8> = Vec::new();
        {
            let mut buf = [0; 8];
            byteutils::write_udbinteger(self.next_rowid, &mut buf);
            new_row.extend_from_slice(&buf); // write row_id into new_row
        }

        self.write_data_to_row(column_data, &mut new_row)?;

        self.rows_set.insert(new_row);
        self.next_rowid += 1;

        Ok(())
    }

    /// Inserts a raw row into the table as is.
    pub fn insert_raw_row(&mut self, raw_row: Vec<u8>) -> Result<bool, UpdateError> {
        Ok(self.rows_set.insert(raw_row))
    }

    /// Writes column data into the new row as bytes. After column data writes
    /// length for records which have a variable length in bytes.
    fn write_data_to_row<I>(
        &mut self,
        column_data: I,
        new_row: &mut Vec<u8>,
    ) -> Result<(), UpdateError>
    where
        I: ExactSizeIterator,
        I: Iterator<Item = (Box<[u8]>, Option<bool>)>,
    {
        trace!(
            "inserting row {} into {} as {:?}",
            self.next_rowid,
            self.name,
            new_row
        );

        let mut lengths = Vec::new();

        for (column, (data_box, is_null)) in self.columns.iter().zip(column_data) {
            let data: &[u8] = &data_box;

            trace!("column data for {}: {:?}", column.name, data);

            let len = data.len() as u64;

            let append_data = match is_null {
                Some(true) => {
                    assert_eq!(len, 0);
                    new_row.push(1);

                    false
                }
                Some(false) => {
                    new_row.push(0);

                    true
                }
                None => true,
            };

            if append_data {
                if column.dbtype.is_valid_length(len) {
                    if column.dbtype.is_variable_length() {
                        let mut buf = [0; 8];
                        byteutils::write_udbinteger(len, &mut buf);
                        lengths.extend_from_slice(&buf);
                    }

                    assert_eq!(column.nullable, is_null.is_some());

                    if let Some(is_null) = is_null {
                        if is_null {}
                    }

                    new_row.extend_from_slice(data);
                } else {
                    return Err(UpdateError::ValidationError {
                        column_name: column.name.clone(),
                    });
                }
            }
        }

        new_row.extend(lengths);
        Ok(())
    }

    /// Deletes one specified row inside a table. Returns 'true' if specified row
    /// was deleted 'false' otherwise.
    pub fn delete_row(&mut self, row: &[u8]) -> Result<bool, UpdateError> {
        Ok(self.rows_set.remove(row))
    }

    /// Deletes the data inside a table, but not the table itself. Returns number
    /// of deleted rows.
    pub fn truncate(&mut self) -> Result<usize, UpdateError> {
        let len = self.rows_set.len();
        self.rows_set.clear();
        Ok(len)
    }

    pub fn get_columns(&self) -> &Vec<Column> {
        &self.columns
    }
}

impl ColumnInfo for Column {
    fn get_offset(&self) -> u32 {
        self.offset
    }
    fn get_name(&self) -> &Identifier {
        &self.name
    }
    fn get_dbtype(&self) -> &DbType {
        &self.dbtype
    }
}
