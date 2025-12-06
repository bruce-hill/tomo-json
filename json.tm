# Base 64 encoding and decoding
use patterns

enum JSONDecodeResult(
    Success(json:JSON)
    Failure(reason:Text)
)
    func invalid(text:Text -> JSONDecodeResult)
        return Failure("Unrecognized JSON: $(text.quoted())")

func quote_text(text:Text -> Text)
    return '"' ++ text.translate({
        "\\": "\\\\",
        '"': '\\"',
        "\f": "\\f",
        "\r": "\\r",
        "\n": "\\n",
        "\b": "\\b",
        "\t": "\\t",
    }) ++ '"'

enum JSON(
    Object(items:{Text:JSON})
    Array(items:[JSON])
    Boolean(value:Bool)
    String(text:Text)
    InvalidUnicodeString(utf32:[Int32])
    Number(n:Num)
    Null
)
    func encode(j:JSON -> Text)
        when j is Object(items)
            return "{" ++ ", ".join([
                '$(quote_text(k)): $(v.encode())'
                for k,v in items
            ]) ++ "}"
        is Array(items)
            return "[" ++ ", ".join([item.encode() for item in items]) ++ "]"
        is Boolean(value)
            return (if value then "true" else "false")
        is String(text)
            return quote_text(text)
        is InvalidUnicodeString(utf32)
            return '"' ++ ((++: (
                if text := Text.from_utf32([u])
                    text
                else
                    "\\u$(u.hex(digits=4, prefix=no))"
            ) for u in utf32) or "") ++ '"'
        is Number(n)
            return "$n"
        is Null
            return "null"

    func pretty_print(j:JSON, max_line:Int=80, indent:Text="  ", current_indent:Text="" -> Text)
        inline := j.encode()
        if inline.length > max_line
            next_indent := current_indent ++ indent
            when j is Object(items)
                return "{\n$next_indent" ++ ",\n$next_indent".join([
                    '$(quote_text(k)): $(v.pretty_print(max_line, indent, next_indent))'
                    for k,v in items
                ]) ++ "\n$current_indent}"
            is Array(items)
                return "[\n$next_indent" ++ ",\n$next_indent".join([item.pretty_print(max_line, indent, next_indent) for item in items]) ++ "\n$current_indent]"
            else pass

        return inline

    func add_codepoint(cur:JSON, codepoint:Int32 -> JSON)
        when cur is String(str)
            if byte_str := Text.from_utf32([codepoint])
                return JSON.String(str ++ byte_str)
            else
                return JSON.InvalidUnicodeString(str.utf32() ++ [codepoint])
        is InvalidUnicodeString(utf32)
            return JSON.InvalidUnicodeString(utf32 ++ [codepoint])
        else
            fail("I expected this to be a String or InvalidUnicodeString, not: $cur")

    func add_text(cur:JSON, text:Text -> JSON)
        when cur is String(str)
            return JSON.String(str ++ text)
        is InvalidUnicodeString(utf32)
            return JSON.InvalidUnicodeString(utf32 ++ text.utf32())
        else
            fail("I expected this to be a String or InvalidUnicodeString, not: $cur")

    func parse_text(text:Text, remainder:&Text?=none, strict=no -> JSONDecodeResult)
        if text.starts_with('"')
            ret := JSON.String("")
            pos := 2
            escapes := {"n":"\n", "t":"\t", "r":"\r", '"':'"', "\\":"\\", "/":"/", "b":"\b", "f":"\f"}
            while pos <= text.length
                c := text[pos]!
                if c == '"'
                    if remainder
                        remainder[] = text.from(pos + 1)
                    return Success(ret)

                if c == "\\"
                    stop if pos + 1 > text.length

                    if esc := escapes[text[pos+1]!]
                        ret = ret.add_text(esc)
                        pos += 2
                    else if m := $Pat'u{4 hex}'.match(text, pos=pos + 1)
                        ret = ret.add_codepoint(Int32.parse(m.captures[1]!, 16)!)
                        pos += 1 + m.text.length
                    else
                        stop
                else if c.utf32()[1]! <= 31
                    if strict
                        return JSONDecodeResult.invalid(text.from(pos))
                    ret = ret.add_text(c)
                    pos += 1
                else
                    ret = ret.add_text(c)
                    pos += 1

        if remainder
            remainder[] = text
        return JSONDecodeResult.invalid(text)

    func parse(text:Text, remainder:&Text?=none, strict=no, max_depth=25 -> JSONDecodeResult)
        if max_depth <= 0
            return JSONDecodeResult.Failure("Maximum depth exceeded")

        if text.starts_with("true", remainder)
            return Success(JSON.Boolean(yes))
        else if text.starts_with("false", remainder)
            return Success(JSON.Boolean(no))
        else if text.starts_with("null", remainder)
            return Success(JSON.Null)

        lower4 := text.to(4).lower()
        if (
            not strict or not (
                lower4.starts_with("+") or
                lower4.starts_with("nan") or
                lower4.starts_with("inf") or
                lower4.starts_with("-inf") or
                lower4.starts_with("-nan") or
                lower4.starts_with("0x") or
                lower4.starts_with("-0x") or
                lower4.starts_with("0o") or
                lower4.starts_with("-0o") or
                lower4.starts_with("0b") or
                lower4.starts_with("-0b")
            )
        )
            if n := Num.parse(text, remainder)
                return Success(JSON.Number(n))

        if text.starts_with('"')
            return JSON.parse_text(text, remainder, strict=strict)
        else if text.starts_with("[")
            elements : &[JSON]
            text = $Pat"{whitespace}".trim(text.from(2), right=no)
            has_trailing_comma := no
            repeat
                when JSON.parse(text, &text, strict=strict, max_depth=max_depth-1) is Success(elem)
                    elements.insert(elem)
                    has_trailing_comma = no
                else stop

                if delim := $Pat'{0+ ws},{0+ ws}'.match(text)
                    text = text.from(delim.text.length + 1)
                    has_trailing_comma = yes
                else stop

            if has_trailing_comma and strict
                return JSONDecodeResult.invalid(text)
                
            if terminator := $Pat'{0+ ws}]'.match(text)
                if remainder
                    remainder[] = text.from(terminator.text.length + 1)
                return Success(JSON.Array(elements))
        else if text.starts_with("{")
            object : &{Text:JSON}
            text = $Pat"{whitespace}".trim(text.from(2), right=no)
            has_trailing_comma := no
            repeat
                key_text := text
                when JSON.parse_text(text, &text, strict=strict) is Success(key_json)
                    key := key_json.String or return JSONDecodeResult.invalid(key_text)
                    if separator := $Pat'{0+ ws}:{0+ ws}'.match(text)
                        text = text.from(separator.text.length + 1)
                    else
                        return JSONDecodeResult.invalid(text)

                    when JSON.parse(text, &text, strict=strict, max_depth=max_depth-1) is Success(value)
                        object[key.text] = value
                        has_trailing_comma = no
                    else
                        return JSONDecodeResult.invalid(text)
                else stop

                if delim := $Pat'{0+ ws},{0+ ws}'.match(text)
                    text = text.from(delim.text.length + 1)
                    has_trailing_comma = yes
                else stop

            if has_trailing_comma and strict
                return JSONDecodeResult.invalid(text)
                
            if terminator := $Pat'{0+ ws}{1}}'.match(text)
                if remainder
                    remainder[] = text.from(terminator.text.length + 1)
                return Success(JSON.Object(object))

        return JSONDecodeResult.invalid(text)

func main(input=(/dev/stdin), pretty_print:Bool = no, strict:Bool = no, max_depth=100)
    text := $Pat"{whitespace}".trim(input.read() or exit("Invalid file: $input"))
    while text.length > 0
        when JSON.parse(text, remainder=&text, strict=strict, max_depth=max_depth) is Success(json)
            if pretty_print
                say(json.pretty_print())
            else
                say(json.encode())

            if strict
                text = $Pat"{whitespace}".trim(text)
                if text.length > 0
                    exit(code=1)
                exit(code=0)

        is Failure(msg)
            exit("\033[31;1m$msg\033[m", code=1)
