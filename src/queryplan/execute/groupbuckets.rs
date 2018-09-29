use databasestorage::Group;
use databasestorage::RawRow;
use std;
use std::borrow::Cow;
use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::IntoIterator;

pub struct GroupBuckets<ColumnValue: Clone + Eq + Hash + 'static> {
    buckets: HashMap<Box<[ColumnValue]>, GroupBucket<ColumnValue>>,
}

impl<ColumnValue: Clone + Eq + Hash + 'static> GroupBuckets<ColumnValue> {
    pub fn new() -> GroupBuckets<ColumnValue> {
        GroupBuckets {
            buckets: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: Box<[ColumnValue]>, row: Box<[ColumnValue]>) {
        if let Some(bucket) = self.buckets.get_mut(&key) {
            bucket.rows.push(row);
            return;
        }

        let bucket = GroupBucket { rows: vec![row] };

        self.buckets.insert(key, bucket);
    }
}

impl<ColumnValue: Clone + Eq + Hash + 'static> IntoIterator for GroupBuckets<ColumnValue> {
    type Item = GroupBucket<ColumnValue>;
    type IntoIter = IntoIter<ColumnValue>;

    fn into_iter(self) -> IntoIter<ColumnValue> {
        IntoIter {
            i: self.buckets.into_iter(),
        }
    }
}

pub struct IntoIter<ColumnValue: Clone + Eq + Hash + 'static> {
    i: std::collections::hash_map::IntoIter<Box<[ColumnValue]>, GroupBucket<ColumnValue>>,
}

impl<ColumnValue: Clone + Eq + Hash + 'static> Iterator for IntoIter<ColumnValue> {
    type Item = GroupBucket<ColumnValue>;

    fn next(&mut self) -> Option<GroupBucket<ColumnValue>> {
        self.i.next().map(|(_k, v)| v)
    }
}

pub struct GroupBucket<ColumnValue: Clone + Eq + Hash + 'static> {
    rows: Vec<Box<[ColumnValue]>>,
}

impl<ColumnValue: Clone + Eq + Hash + 'static> Group for GroupBucket<ColumnValue> {
    type ColumnValue = ColumnValue;

    fn get_any_row(&self) -> Option<Cow<[ColumnValue]>> {
        self.rows.iter().nth(0).map(|r| Cow::from(r.as_ref()))
    }

    fn get_any_raw_row(&self) -> Option<RawRow<<Self as Group>::ColumnValue>> {
        panic!("GroupBucket can't get row as raw bytes!")
    }

    fn count(&self) -> u64 {
        self.rows.len() as u64
    }

    fn iter<'a>(&'a self) -> Box<Iterator<Item = Cow<'a, [ColumnValue]>> + 'a> {
        Box::new(self.rows.iter().map(|row| {
            let row_ref: &[ColumnValue] = &row;
            row_ref.into()
        }))
    }

    fn iter_raw<'a>(&'a self) -> Box<Iterator<Item = RawRow<<Self as Group>::ColumnValue>> + 'a> {
        panic!("GroupBucket can't get iterator of raw bytes!")
    }
}
