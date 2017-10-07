module StringDiff =
struct
  type t = string
  let compare = String.compare
  let pp_printer = Format.pp_print_string
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module RefDiff(RefType : sig type t end) = struct
  type t = RefType.t Ref.t
  let compare = compare
  let pp_printer formatter ref = Format.pp_print_string formatter (Ref.string_of ref)
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module StringSet = OUnitDiff.SetMake(StringDiff)
module StringList = OUnitDiff.ListSimpleMake(StringDiff)
module RefList(RefType : sig type t end) = OUnitDiff.ListSimpleMake(RefDiff(RefType))
