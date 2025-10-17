# Handy vim-python script for converting a C structure into an OCaml module
# e.g. configure Shift-middle-click to convert the selection to OCaml and paste:
#   noremap <S-MiddleMouse> <LeftMouse><Cmd>py3file ./convert.py<CR>

import re

c_code = vim.eval('@*')

if 0:
    c_code = '''
        struct _drmPciBusInfo {
            uint16_t domain;
            uint8_t bus;
            uint8_t dev;
            uint8_t func;
        }
    '''

c_code = re.sub(r"/\*.*?\*/", "", c_code)
#print(c_code)

c_code = [l.strip() for l in c_code.split('\n')]
c_code = [l for l in c_code if l]

cursor_line = vim.current.window.cursor[0] - 1

ocaml_mod = {
    'char': 'Char',
    'int': 'Int',
    'uint8_t': 'Unsigned.UInt8',
    'uint16_t': 'Unsigned.UInt16',
    'uint32_t': 'Unsigned.UInt32',
    'uint64_t': 'Unsigned.UInt64',
}

safe_name = {
    'type': '_type',
}

assert c_code[-1].startswith("}"), c_code
bits = c_code[0].split()
if bits[0] == "typedef": bits = bits[1:]
if bits[0] == "struct":
    str_ty = "structure"
elif bits[0] == "union":
    str_ty = "union"
else:
    assert False, str_ty
if bits[1] == '{':
    bits = c_code[-1].split()
    struct_name = bits[1].strip(';')
else:
    struct_name = bits[1]
    assert (bits[2] == "{"), bits

x = struct_name.strip('_')
module_name = x[0].upper() + x[1:]

lines = [
    f"module {module_name} = struct",
    f"  type mark",
    f"  type ctype = mark Ctypes.{str_ty}",
    f'  let t : ctype F.typ = F.{str_ty} "{struct_name}"',
    f'',
]

class Ty:
    def __init__(self, x):
        self.name = x

    def ocaml_ctype(self):
        ty = self.name
        if ty in ocaml_mod:
            return f'F.{ty}'
        else:
            mod = ty[0].upper() + ty[1:]
            return f'{mod}.t'

    def ocaml_type(self):
        if self.name in ocaml_mod: return "int"
        else: return self.ocaml_ctype()

    def to_ocaml(self, v):
        ty = self.name
        if ty == "int":
            return v
        else:
            mod = ocaml_mod.get(ty, None)
            if mod:
                return f'{mod}.to_int ({v})'
            else:
                mod = ty[0].upper() + ty[1:]
                return f'{mod}.of_c ({v})'

class Ptr:
    def __init__(self, x):
        self.target = x

    def ocaml_ctype(self):
        return f'(F.ptr {self.target.ocaml_ctype()})'

    def ocaml_type(self):
        return self.target.ocaml_type()

    def to_ocaml(self, v):
        return self.target.to_ocaml(f'Ctypes.(!@) ({v})')

class Array:
    def __init__(self, elt, size):
        self.elt = elt
        self.size = size

    def length(self):
        if self.size[0].isdigit():
            return self.size
        else:
            return f'Config.{self.size.lower()}'

    def ocaml_ctype(self):
        return f'(F.array {self.length()} {self.elt.ocaml_ctype()})'

    def to_ocaml(self, v):
        elt = self.elt.ocaml_ctype()
        if elt == "Ctypes.char": return f'string_of_carray ({v})'
        else: return f'(Ctypes.CArray.to_list {v})'

    def ocaml_type(self):
        elt = self.elt.ocaml_ctype()
        if elt == "Ctypes.char": return "string"
        else: return f'{elt} list'

raw_fields = []
for line in c_code[1:-1]:
    ty, names = line.strip(' \t;').split(maxsplit = 1)
    if ty.endswith("Ptr"):
        ty = Ptr(Ty(ty[:-3]))
    else:
        ty = Ty(ty)
    for field in names.split(','):
        field = field.strip()
        while field[0] == "*":
            field = field[1:]
            ty = Ptr(ty)
        while field[-1] == "]":
            field, size = field[:-1].split('[')
            ty = Array(ty, size)
        raw_fields.append((field, ty))

fields = []
for c_name, ty in raw_fields:
    name = safe_name.get(c_name, c_name)
    lines.append(f'  let {name} = F.field t "{c_name}" {ty.ocaml_ctype()}')
    fields.append((name, ty))

lines.append('  let () = F.seal t')

lines.append('end')

if str_ty == "structure":
    lines.append('')
    assert module_name.startswith('Drm'), module_name
    lines.append(f"module {module_name[3:]} = struct")
    lines.append(f"  open CT.{module_name}")
    lines.append('  type t = {')
    for field, ty in fields:
        otype = ty.ocaml_type()
        lines.append(f'    {field} : {otype};')
    lines.append('  }')
    lines.append('  let of_c c = {')
    for field, ty in fields:
        v = f'Ctypes.getf c {field}'
        lines.append(f'    {field} = {ty.to_ocaml(v)};')
    lines.append('  }')
    lines.append('  let pp f t =')
    fmt = ';@ '.join([f'{field} = %d' for field, ty in fields])
    lines.append('    Fmt.pf f "{@[<hv>' + fmt + '@]}"')
    values = ' '.join([f't.{field}' for field, ty in fields])
    lines.append('      ' + values)

lines.append('end')

b = vim.current.buffer
before = b[cursor_line - 1]
after = b[cursor_line]
if after: b[cursor_line:cursor_line] = [""]

#for l in lines: print(l)
b[cursor_line:cursor_line] = [('  ' + l).rstrip() for l in lines]
if before: b[cursor_line:cursor_line] = [""]
