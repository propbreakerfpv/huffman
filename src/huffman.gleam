import gleam/io
import gleam/int
import gleam/dict.{type Dict}
import gleam/string
import gleam/list
import gleam/bit_array
import file_streams/write_stream
import file_streams/read_stream
import simplifile
import argv

pub type Tree {
  Leaf(freq: Int, char: String)
  Node(freq: Int, lhs: Tree, rhs: Tree)
}

pub fn main() {
  let args = argv.load().arguments
  case args {
    ["decode", path] -> {
      let assert Ok(rs) = read_stream.open(path)
      let assert Ok(bits) = read_stream.read_bytes(rs, 10000000) 
      let assert Ok(Nil) = read_stream.close(rs)

      
      let text = huffman_decode(bits)
      io.println("decoded text")
      io.print(text)
    }
    _ -> {
      let assert Ok(content) = simplifile.read("README.md")
      huffman_encode(content)
      |> write_encoded
      Nil
    }
  }
}

const version = 0x0001

fn huffman_decode(bits: BitArray) -> String {

  let #(_version, bits) = decode_int(bits, 16)
  let #(table_size, bits) = decode_int(unwrap(bits), 16)
  let #(text_size, bits) = decode_int(unwrap(bits), 32)
  let assert Ok(encoded_table) = bit_array.slice(unwrap(bits), 0, table_size)
  let assert Ok(encoded_text) = bit_array.slice(unwrap(bits), table_size, text_size)
  let table = decode_table(encoded_table)

  decode_text(encoded_text, table)
}

fn decode_text(bits: BitArray, table: Dict(String, #(Int, Int))) -> String {
  let inv_table = invert(table)
  let text = decode_text_loop(bits, inv_table)
  text
}

fn decode_text_loop(bits: BitArray, inv_table: Dict(#(Int, Int), String)) -> String {
  case find_code(bits, inv_table, 0, 0) {
    #("", _) -> ""
    #(char, bits) -> {
      char <> decode_text_loop(bits, inv_table)
    }
  }
}

fn find_code(bits: BitArray, inv_table: Dict(#(Int, Int), String), acc: Int, depth: Int) -> #(String, BitArray) {
  case bits {
    <<bit:size(1), rest:bits>> -> {
      let key = int.bitwise_shift_left(acc, 1) + bit
      case dict.get(inv_table, #(key, depth + 1)) {
        Ok(char) -> #(char, rest)
        Error(_) -> find_code(rest, inv_table, key, depth + 1)
      }
    }
    _ -> #("", <<>>)
  }
}


fn decode_table(bits: BitArray) -> Dict(String, #(Int, Int)) {
  let #(char, bits) = decode_char(bits)
  case bits {
    Ok(bits) -> {
      let #(len, bits) = decode_int(bits, 8)
      case bits {
        Ok(bits) -> {
          let #(code, bits) = decode_int(bits, len)
          case bits {
            Ok(bits) -> {
              dict.new()
              |> dict.insert(_, char, #(code, len))
              |> dict.merge(_, decode_table(bits))
            }
            Error(_) -> dict.new()
      }
          }
        Error(_) -> dict.new()
      }
    }
    Error(_) -> dict.new()
  }
}

fn decode_char(bits: BitArray) -> #(String, Result(BitArray, Nil)) {
  case bits {
    <<byte:utf8_codepoint, rest:bits>> -> #(string.from_utf_codepoints([byte]), Ok(rest))
    <<byte:utf8_codepoint>> -> #(string.from_utf_codepoints([byte]), Error(Nil))
    _ -> #("", Error(Nil))
  }
}

fn decode_int(bits: BitArray, size: Int) -> #(Int, Result(BitArray, Nil)) {
  case bits {
    <<byte:size(size), rest:bits>> -> #(byte, Ok(rest))
    _ -> #(0, Error(Nil))
  }
}

fn huffman_encode(content: String) -> BitArray {
  let freq = get_freq(content)
  let tree = make_tree(freq)
  let table = make_table(tree)
  let #(encoded, len) = encode(content, table)
  let size = 8 - len % 8
  let encoded = bit_array.append(encoded, <<0:size(size)>>)

  make_encoded(encoded, table)
}

fn display_bin(bits: BitArray) {
  let #(n, rest) = decode_int(bits, 8)
  io.println(int.to_base2(n))
  case rest {
    Ok(rest) -> display_bin(rest)
    _ -> Nil
  }
}

fn make_encoded(array: BitArray, table: Dict(String, #(Int, Int))) -> BitArray {
  // what        size
  // version:    16
  // table size: 16
  // text size:  32
  // table:      table size in bytes
  // text:       text size in bytes

  // table and text are padded to to align to bytes

  let text_size = bit_array.byte_size(array)
  let #(encoded_table, table_size) = encode_table(table)
  let table_offset = 8 - table_size % 8
  let table_size_bytes = table_size + table_offset
  let table_size_bytes = table_size_bytes / 8


  <<>>
  |> bit_array.append(_, <<version:size(16)>>)
  |> bit_array.append(_, <<table_size_bytes:size(16)>>)
  |> bit_array.append(_, <<text_size:size(32)>>)
  |> bit_array.append(_, encoded_table)
  |> bit_array.append(_, <<0:size(table_offset)>>)
  |> bit_array.append(_, array)
}
fn write_encoded(bits: BitArray) {
  let assert Ok(ws) = write_stream.open("out.bin")
  let assert Ok(Nil) = write_stream.write_bytes(ws, bits)
  let assert Ok(Nil) = write_stream.close(ws)
}

fn encode_table(table: Dict(String, #(Int, Int))) -> #(BitArray, Int) {
  let keys = dict.keys(table)
  case list.first(keys) {
    Ok(key) -> {
      let assert Ok(code) = dict.get(table, key)
      let #(bits, len) = encode_table(dict.delete(table, key))
      let bits = <<key:utf8, code.1:size(8), code.0:size(code.1)>>
      |> bit_array.append(_, bits)
      #(bits, len + 8 + 8 + code.1)
    }
    Error(_) -> {
      #(<<>>, 0)
    }
  }
}

fn encode(content: String, table: Dict(String, #(Int, Int))) -> #(BitArray, Int) {
  case string.first(content) {
    Ok(char) -> {
      let assert Ok(code) = dict.get(table, char)
      let #(bits, len) = encode(string.drop_left(content, 1), table)
      #(bit_array.append(<<code.0:size(code.1)>>, bits), len + code.1)
    }
    Error(_) -> {
      #(<<>>, 0)
    }
  }
}

fn find_bit_len(n: Int) -> Int {
  case n {
    n if n > 1 -> {
      1 + find_bit_len(n / 2)
    }
    1 -> 1
    0 -> 0
    _ -> panic
  }
}

fn make_table(tree: Tree) -> Dict(String, #(Int, Int)) {
  case tree {
    Leaf(_, _) -> panic
    Node(_, lhs, rhs) -> {
      let lhs = make_table_rec(lhs, 0, 1)
      let rhs = make_table_rec(rhs, 1, 1)
      dict.merge(lhs, rhs)
    }
  }
}

fn make_table_rec(tree: Tree, hist: Int, depth: Int) -> Dict(String, #(Int, Int)) {
  case tree {
    Leaf(_, char) -> {
      let d = dict.new()
      dict.insert(d, char, #(hist, depth))
    }
    Node(_, lhs, rhs) -> {
      let lhs = make_table_rec(lhs, hist * 2, depth + 1)
      let rhs = make_table_rec(rhs, hist * 2 + 1, depth + 1)
      dict.merge(lhs, rhs)
    }
  }
}

fn make_tree(freq: Dict(String, Int)) -> Tree {
  let freq = dict.to_list(freq)
  let freq = list.sort(freq, fn(a, b) {int.compare(a.1, b.1)})
  let freq = list.map(freq, fn(x) {
    Leaf(char: x.0, freq: x.1)
  })
  case make_tree_loop(freq) {
    [t] -> t
    _ -> {
      io.debug(freq)
      io.debug(list.first(freq))
      panic as "expected table list to be length 1"
    }
  }
}
fn make_tree_loop(freq: List(Tree)) -> List(Tree) {
  case list.pop(freq, fn(_){True}) {
    Ok(#(lhs, freq)) -> case list.pop(freq, fn(_){True}) {
      Ok(#(rhs, freq)) -> {
        let freq = list.append(freq, [Node(lhs: lhs, rhs: rhs, freq: lhs.freq + rhs.freq)])
        list.sort(freq, fn(a, b){int.compare(a.freq, b.freq)})
        |> make_tree_loop()
      }
      Error(_) -> {
        [lhs]
      }
    }
    Error(_) -> freq
  }
}

fn get_freq(content: String) -> Dict(String, Int) {
  case string.first(content) {
    Ok(char) -> {
      let d = dict.new()
      let d = dict.insert(d, char, 1)
      dict_join(d, get_freq(string.drop_left(content, 1)), fn(v1, v2) {
        let v1 =case v1 {
          Ok(v) -> v
          Error(_) -> 0
        }
        let v2 =case v2 {
          Ok(v) -> v
          Error(_) -> 0
        }
        v1 + v2
      })
    }
    Error(_) -> dict.new()
  }
}

fn dict_join(d1: Dict(key, value), d2: Dict(key, value), f: fn(Result(value, Nil), Result(value, Nil)) -> value) -> Dict(key, value) {
  let keys = dict.keys(d1)
  dict_join_loop(d1, d2, f, keys)
}

fn dict_join_loop(d1: Dict(key, value), d2: Dict(key, value), f: fn(Result(value, Nil), Result(value, Nil)) -> value, keys: List(key)) -> Dict(key, value) {
  case list.pop(keys, fn(_) {True}) {
    Ok(#(key, l)) -> {
      let d = dict_join_loop(d1, dict.drop(d2, [key]), f, l)
      dict.insert(d, key, f(dict.get(d1, key), dict.get(d2, key)))
    }
    Error(_) -> d2
  }
}

fn unwrap(res: Result(a, b)) -> a {
  case res {
    Ok(v) -> v
    Error(_) -> panic
  }
}
fn invert(d: Dict(a, b)) -> Dict(b, a) {
  case dict.keys(d) |> list.first() {
    Ok(key) -> {
      case dict.keys(d) {
        [key, ..] -> {
          let assert Ok(value) = dict.get(d, key)
          dict.new()
          |> dict.insert(_, value, key)
          |> dict.merge(_, invert(dict.delete(d, key)))
        }
        _ -> {
          let assert Ok(value) = dict.get(d, key)
          dict.new()
          |> dict.insert(_, value, key)
        }
      }
    }
    Error(_) -> {
      dict.new()
    }
  }
}

fn compare_table(table1: Dict(String, #(Int, Int)), table2: Dict(String, #(Int, Int))) {
  case dict.keys(table1) {
    [key, ..] -> {
      let assert Ok(value1) = dict.get(table1, key)
      let assert Ok(value2) = dict.get(table2, key)
      io.print(key)
      io.print(": ")
      io.print(int.to_base2(value1.0))
      io.print(", ")
      io.print(int.to_string(value1.1))
      io.print(" --- ")
      io.print(key)
      io.print(": ")
      io.print(int.to_base2(value2.0))
      io.print(", ")
      io.println(int.to_string(value2.1))
      compare_table(dict.delete(table1, key), dict.delete(table1, key))
      Nil
    }
    [] -> {
      Nil
    }
  }
}

fn display_len_table(table: Dict(String, #(Int, Int))) {
  case dict.keys(table) {
    [key, ..] -> {
      let assert Ok(value) = dict.get(table, key)
      io.print(key)
      io.print(": ")
      io.print(int.to_base2(value.0))
      io.print(", ")
      io.debug(value.1)
      display_len_table(dict.delete(table, key))
      Nil
    }
    [] -> {
      Nil
    }
  }
}
fn display_table(table: Dict(String, Int)) {
  case dict.keys(table) {
    [key, ..] -> {
      let assert Ok(value) = dict.get(table, key)
      io.print(key)
      io.print(": ")
      io.println(int.to_base2(value))
      display_table(dict.delete(table, key))
      Nil
    }
    [] -> {
      Nil
    }
  }
}
