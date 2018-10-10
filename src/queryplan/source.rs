use identifier::Identifier;

pub struct TableOrSubquery {
    pub source_id: u32,
    pub out_column_names: Vec<Identifier>,
}

pub struct SourceScope<'a> {
    parent: Option<&'a SourceScope<'a>>,
    pub tables: Vec<TableOrSubquery>,
    pub table_aliases: Vec<Identifier>,
}

pub enum GetColumnOffsetResult {
    One((u32, u32)),
    None,
    Ambiguous(Vec<(u32, u32)>),
}

fn get_candidates<'a, I>(tables: I, name: &Identifier) -> Vec<(u32, u32)>
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
                .map(move |offset| (source_id, offset))
        }).collect()
}

impl<'a> SourceScope<'a> {
    pub fn new(
        parent: Option<&'a SourceScope<'a>>,
        tables: Vec<TableOrSubquery>,
        table_aliases: Vec<Identifier>,
    ) -> SourceScope<'a> {
        SourceScope {
            parent: parent,
            tables: tables,
            table_aliases: table_aliases,
        }
    }

    pub fn tables(&self) -> &[TableOrSubquery] {
        &self.tables
    }

    pub fn get_column_offset(&self, column_name: &Identifier) -> GetColumnOffsetResult {
        let candidates = self.get_column_offsets(column_name);

        match candidates.len() {
            0 => GetColumnOffsetResult::None,
            1 => GetColumnOffsetResult::One(candidates[0]),
            _ => GetColumnOffsetResult::Ambiguous(candidates),
        }
    }

    pub fn get_table_column_offset(
        &self,
        table_name: &Identifier,
        column_name: &Identifier,
    ) -> GetColumnOffsetResult {
        let candidates = self.get_table_column_offsets(table_name, column_name);

        match candidates.len() {
            0 => GetColumnOffsetResult::None,
            1 => GetColumnOffsetResult::One(candidates[0]),
            _ => GetColumnOffsetResult::Ambiguous(candidates),
        }
    }

    fn get_column_offsets(&self, column_name: &Identifier) -> Vec<(u32, u32)> {
        let mut candidates = get_candidates(self.tables.iter(), column_name);

        if let Some(parent) = self.parent {
            candidates.extend(parent.get_column_offsets(column_name));
        }

        candidates
    }

    fn get_table_column_offsets(
        &self,
        table_name: &Identifier,
        column_name: &Identifier,
    ) -> Vec<(u32, u32)> {
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
