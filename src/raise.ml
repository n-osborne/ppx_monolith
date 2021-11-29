open Ppxlib

let name = "ppx_monolith"

module Unsupported = struct
  let coretype ~loc ty =
    Location.raise_errorf ~loc "%s: %s type not supported" name ty

  let typekind ~loc kind =
    Location.raise_errorf ~loc "%s: %s type kind not supported" name kind

  let longident ~loc id =
    Location.raise_errorf ~loc "%s: %s longident not supported." name id

  let constructor ~loc cstr =
    Location.raise_errorf ~loc "%s: %s constructor not supported." name cstr
end
