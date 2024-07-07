 # Architecture
 This document describes the architecture of the virtual machine.

 ## Instructions
| Name             | Opcode | Operands |
| ---------------- | ------ | -------- |
| `return`         | 0x00   | void     |
| `const`          | 0x10   | u16      |
| `pop`            | 0x20   | void     |
| `jump`           | 0x30   | u16      |
| `jump_if_false`  | 0x40   | u16      |
| `true`           | 0x10   | void     |
| `false`          | 0x11   | void     |
| `null`           | 0x12   | void     |
| `void`           | 0x13   | void     |
| `add`            | 0x20   | void     |
| `sub`            | 0x21   | void     |
| `mul`            | 0x22   | void     |
| `div`            | 0x23   | void     |
| `mod`            | 0x24   | void     |
| `pow`            | 0x25   | void     |
| `neg`            | 0x26   | void     |
| `eql`            | 0x30   | void     |
| `neql`           | 0x31   | void     |
| `lt`             | 0x32   | void     |
| `lt_eql`         | 0x33   | void     |
| `gt`             | 0x34   | void     |
| `gt_eql`         | 0x35   | void     |
| `and`            | 0x40   | void     |
| `or`             | 0x41   | void     |
| `not`            | 0x42   | void     |
| `call_builtin`   | 0x50   | u16, u16 |
| `declare_global` | 0x60   | u16      |
| `set_global`     | 0x61   | u16      |
| `get_global`     | 0x62   | u16      |
| `set_local`      | 0x70   | u16      |
| `get_local`      | 0x71   | u16      |
