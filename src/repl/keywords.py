from ...api import edn

TAG = edn.Keyword("tag")
ERR = edn.Keyword("err")
OUT = edn.Keyword("out")
IN = edn.Keyword("in")
PROMPT = edn.Keyword("prompt")
RET = edn.Keyword("ret")
VAL = edn.Keyword("val")
TAP = edn.Keyword("tap")

OUTPUT = edn.Keyword("output")
VIEW = edn.Keyword("view")
CLIPBOARD = edn.Keyword("clipboard")
COMMENT = edn.Keyword("comment")
STRING = edn.Keyword("string")
INLINE = edn.Keyword("inline")
POINT = edn.Keyword("point")
VIEW_ID = edn.Keyword("view-id")
