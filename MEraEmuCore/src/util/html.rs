pub struct SplitHtmlIterator<'a> {
    src: &'a str,
}
impl<'a> Iterator for SplitHtmlIterator<'a> {
    type Item = Result<&'a str, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.src.is_empty() {
            return None;
        }
        let Some(tag_start_pos) = self.src.find('<') else {
            return Some(Ok(std::mem::take(&mut self.src)));
        };
        let part;
        if tag_start_pos > 0 {
            (part, self.src) = self.src.split_at(tag_start_pos);
            return Some(Ok(part));
        }
        let Some(tag_end_pos) = self.src.find('>') else {
            return Some(Err(()));
        };
        (part, self.src) = self.src.split_at(tag_end_pos + 1);
        Some(Ok(part))
    }
}

pub fn split_html_tags<'a>(src: &'a str) -> SplitHtmlIterator<'a> {
    SplitHtmlIterator { src }
}
