use databaseinfo::DatabaseInfo;
use std::borrow::Cow;
use std::cmp::Eq;
use std::hash::Hash;

pub trait DatabaseStorage {
    type Info: DatabaseInfo;

    fn scan_table<'a>(
        &'a self,
        table: &'a <Self::Info as DatabaseInfo>::Table,
    ) -> Box<Group<ColumnValue = <Self::Info as DatabaseInfo>::ColumnValue> + 'a>;
}

pub struct RawRow<'a, ColumnValue: Sized + Clone + Eq + Hash + 'static> {
    pub row: Cow<'a, [ColumnValue]>,
    pub raw_row: &'a Vec<u8>,
}

pub trait Group {
    type ColumnValue: Sized + Clone + Eq + Hash + 'static;

    /// Returns any arbitrary row in the group.
    /// Returns None if the group contains no rows.
    fn get_any_row(&self) -> Option<Cow<[Self::ColumnValue]>>;

    fn get_any_raw_row(&self) -> Option<RawRow<Self::ColumnValue>>;

    fn count(&self) -> u64;

    fn iter<'a>(&'a self) -> Box<Iterator<Item = Cow<'a, [Self::ColumnValue]>> + 'a>;

    fn iter_raw<'a>(&'a self) -> Box<Iterator<Item = RawRow<Self::ColumnValue>> + 'a>;
}
