use lsp_types::Position;
use lsp_types::Range;
use std::collections::VecDeque;
use std::io::Read;

pub struct CodeReader<Reader: Read> {
  position: Position,
  data: Reader,
  peekQueue: VecDeque<(u8, Position)>,
}

impl<Reader: Read> CodeReader<Reader> {
  pub fn new(r: Reader) -> Self {
    CodeReader {
      position: Position {
        line: 0,
        character: 0,
      },
      data: r,
      peekQueue: VecDeque::new(),
    }
  }

  pub fn dropc(&mut self) {
    self.getc();
  }

  pub fn getc(&mut self) -> Option<u8> {
    if let Some((c, _)) = self.peekQueue.pop_front() {
      return Some(c)
    }
    else {
      // the peek queue is empty
      self
        .data
        .bytes()
        .next()
        .and_then(|result| result.ok())
        .map(|byte| {
          // handle managing the line position
          if byte == '\n' as u8 {
            self.position.line += 1;
            self.position.character = 0;
          } else {
            self.position.character += 1;
          }
          byte
        })
    }
  }

  pub fn position(&self) -> Position {
    if let Some((_, pos)) = self.peekQueue.get(0) {
        *pos
    } else {
        self.position
    }
  }

  pub fn peekpos(&mut self, n: u32) -> Option<Position> {

  }

  pub fn peekc(&mut self, n: u32) -> Option<u8> {

  }
}
