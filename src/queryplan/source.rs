use identifier::Identifier;

/// Representation for Table or Subquery
pub struct TableOrSubquery {
    /// Id for this Source of data
    pub source_id: u32,

    /// Names of all columns in this Source
    pub out_column_names: Vec<Identifier>,
}

/// Wrapper for source id of [TableOrSubquery] and serial number of this
/// [TableOrSubquery].
#[derive(Clone)]
pub struct SourceIdAndOffset(pub u32, pub u32);

pub enum GetColumnOffsetResult {
    One(SourceIdAndOffset),
    None,
    Ambiguous(Vec<SourceIdAndOffset>),
}

/// Scope of Source
pub struct SourceScope<'a> {
    parent: Option<&'a SourceScope<'a>>,
    pub tables: Vec<TableOrSubquery>,
    pub table_aliases: Vec<Identifier>,
}

impl<'a> SourceScope<'a> {
    pub fn new(
        parent: Option<&'a SourceScope<'a>>,
        tables: Vec<TableOrSubquery>,
        table_aliases: Vec<Identifier>,
    ) -> SourceScope<'a> {
        SourceScope {
            parent,
            tables,
            table_aliases,
        }
    }

    /// Returns table of this [SourceScope].
    pub fn tables(&self) -> &[TableOrSubquery] {
        &self.tables
    }

    /// Finds recursively all columns offsets for all tables by specified
    /// `column_name` in this scope and all parents scopes.
    pub fn get_column_offset(&self, column_name: &Identifier) -> GetColumnOffsetResult {
        let candidates = self.get_column_offsets(column_name);

        match candidates.len() {
            0 => GetColumnOffsetResult::None,
            1 => GetColumnOffsetResult::One(candidates[0].clone()),
            _ => GetColumnOffsetResult::Ambiguous(candidates),
        }
    }

    /// Finds recursively all columns offsets for specified table and
    /// `column_name` in this scope and all parents scopes.
    pub fn get_table_column_offset(
        &self,
        table_name: &Identifier,
        column_name: &Identifier,
    ) -> GetColumnOffsetResult {
        let candidates = self.get_table_column_offsets(table_name, column_name);

        match candidates.len() {
            0 => GetColumnOffsetResult::None,
            1 => GetColumnOffsetResult::One(candidates[0].clone()),
            _ => GetColumnOffsetResult::Ambiguous(candidates),
        }
    }

    fn get_column_offsets(&self, column_name: &Identifier) -> Vec<SourceIdAndOffset> {
        let mut candidates = get_candidates(self.tables.iter(), column_name);

        // if parent is defined add candidates from parent too
        if let Some(parent) = self.parent {
            candidates.extend(parent.get_column_offsets(column_name));
        }

        candidates
    }

    fn get_table_column_offsets(
        &self,
        table_name: &Identifier,
        column_name: &Identifier,
    ) -> Vec<SourceIdAndOffset> {
        let tables = self
            .table_aliases
            .iter()
            .enumerate()
            .filter_map(|(i, name)| {
                if name == table_name {
                    Some(&self.tables[i])
                } else {
                    None
                }
            });

        let mut candidates = get_candidates(tables, column_name);

        if let Some(parent) = self.parent {
            candidates.extend(parent.get_table_column_offsets(table_name, column_name));
        }

        candidates
    }
}

/// Finds all [TableOrSubquery] in `tables` that contain column `name` inside and
/// returns [SourceIdAndOffset] for found [TableOrSubquery].
fn get_candidates<'a, I>(tables: I, name: &Identifier) -> Vec<SourceIdAndOffset>
where
    I: Iterator<Item = &'a TableOrSubquery>,
{
    tables
        .flat_map(|table| {
            let source_id = table.source_id;

            table
                .out_column_names
                .iter()
                .enumerate()
                .filter_map(|(i, ident)| if ident == name { Some(i as u32) } else { None })
                .map(move |offset| SourceIdAndOffset(source_id, offset))
        })
        .collect()
}
