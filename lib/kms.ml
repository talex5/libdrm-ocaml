module CT = C.Types
module U32 = Unsigned.UInt32
module U64 = Unsigned.UInt64
module String_map = Map.Make(String)

let ( !@ ) = Ctypes.( !@ )

let string_of_carray (a : char Ctypes.CArray.t) =
  let rec aux i =
    if i = a.alength then i
    else if Ctypes.CArray.get a i = '\000' then i
    else aux (i + 1)
  in
  let length = aux 0 in
  Ctypes.string_from_ptr a.astart ~length

let get_array c arr_field len_field =
  let arr = Ctypes.getf c arr_field in
  let len = Ctypes.getf c len_field in
  let a = Ctypes.CArray.from_ptr arr len in
  Ctypes.CArray.to_list a

let pp_limited n pp f xs =
  if List.compare_length_with xs n > 0 then
    Fmt.pf f "[@[<hv>%a;@ ...@]]" (Fmt.(list ~sep:semi) pp) (List.take n xs)
  else
    Fmt.Dump.list pp f xs

module type BITSET = sig
  type t = private U32.t

  val empty : t
  val ( + ) : t -> t -> t
  val mem : t -> t -> bool
  val of_uint32 : U32.t -> t
  val pp : t Fmt.t
end

module Bitset = struct
  type t = U32.t

  let empty = U32.zero

  let ( + ) = U32.logor

  let mem flag x =
    U32.logand x flag = flag

  let of_uint32 x = x

  let pp names f t =
    if t = U32.zero then Fmt.string f "0"
    else (
      let t = ref t in
      let flag (v, name) =
        if mem v !t then (
          t := U32.sub !t v;
          Some name
        ) else None
      in
      let items = List.filter_map flag names in
      let items =
        if !t = U32.zero then items
        else Fmt.str "0x%a" U32.pp_hex !t :: items
      in
      Fmt.(list ~sep:(any "+") string) f items
    )
end

module Mode_info = struct
  open CT.DrmModeModeInfo

  module Type = struct
    include Bitset
    include Type

    let values =
      [
        builtin, "builtin";
        clock_c, "clock_c";
        crtc_c, "crtc_c";
        preferred, "preferred";
        default, "default";
        userdef, "userdef";
        driver, "driver";
      ]

    let pp = pp values
  end

  module Flags = struct
    include Bitset
    include Flags

    let values =
      [
        phsync, "phsync";
        nhsync, "nhsync";
        pvsync, "pvsync";
        nvsync, "nvsync";
        interlace, "interlace";
        dblscan, "dblscan";
        csync, "csync";
        pcsync, "pcsync";
        ncsync, "ncsync";
        hskew, "hskew";
        bcast, "bcast";
        pixmux, "pixmux";
        dblclk, "dblclk";
        clkdiv2, "clkdiv2";
      ]

    let pp = pp values
  end

  module Stereo_mode = struct
    open Stereo_mode

    type t =
      | None
      | Frame_packing
      | Field_alternative
      | Line_alternative
      | Side_by_side_full
      | L_depth
      | L_depth_gfx_gfx_depth
      | Top_and_bottom
      | Side_by_side_half
      | Unknown of U32.t

    let values =
      [
        none, None;
        frame_packing, Frame_packing;
        field_alternative, Field_alternative;
        line_alternative, Line_alternative;
        side_by_side_full, Side_by_side_full;
        l_depth, L_depth;
        l_depth_gfx_gfx_depth, L_depth_gfx_gfx_depth;
        top_and_bottom, Top_and_bottom;
        side_by_side_half, Side_by_side_half;
      ]

    let of_c c =
      List.assoc_opt c values |> Option.value ~default:(Unknown c)

    let to_c = function
      | None -> none
      | Frame_packing -> frame_packing
      | Field_alternative -> field_alternative
      | Line_alternative -> line_alternative
      | Side_by_side_full -> side_by_side_full
      | L_depth -> l_depth
      | L_depth_gfx_gfx_depth -> l_depth_gfx_gfx_depth
      | Top_and_bottom -> top_and_bottom
      | Side_by_side_half -> side_by_side_half
      | Unknown x -> x

    let to_string = function
      | None -> "None"
      | Frame_packing -> "Frame_packing"
      | Field_alternative -> "Field_alternative"
      | Line_alternative -> "Line_alternative"
      | Side_by_side_full -> "Side_by_side_full"
      | L_depth -> "L_depth"
      | L_depth_gfx_gfx_depth -> "L_depth_gfx_gfx_depth"
      | Top_and_bottom -> "Top_and_bottom"
      | Side_by_side_half -> "Side_by_side_half"
      | Unknown x -> U32.to_string x

    let pp = Fmt.of_to_string to_string
  end

  module Aspect_ratio = struct
    open Aspect_ratio

    type t =
      | R_none
      | R_4_3
      | R_16_9
      | R_64_27
      | R_256_135
      | Unknown of U32.t

    let values =
      [
        ar_none, R_none;
        ar_4_3, R_4_3;
        ar_16_9, R_16_9;
        ar_64_27, R_64_27;
        ar_256_135, R_256_135;
      ]

    let of_c c =
      List.assoc_opt c values |> Option.value ~default:(Unknown c)

    let to_c = function
      | R_none -> ar_none
      | R_4_3 -> ar_4_3
      | R_16_9 -> ar_16_9
      | R_64_27 -> ar_64_27
      | R_256_135 -> ar_256_135
      | Unknown x -> x

    let to_string = function
      | R_none -> "None"
      | R_4_3 -> "4:3"
      | R_16_9 -> "16:9"
      | R_64_27 -> "64:27"
      | R_256_135 -> "256:135"
      | Unknown x -> U32.to_string x

    let pp = Fmt.of_to_string to_string
  end

  type t = {
    clock : int;
    hdisplay : int;
    hsync_start : int;
    hsync_end : int;
    htotal : int;
    hskew : int;
    vdisplay : int;
    vsync_start : int;
    vsync_end : int;
    vtotal : int;
    vscan : int;
    vrefresh : int;
    flags : Flags.t;
    stereo_mode : Stereo_mode.t;
    aspect_ratio : Aspect_ratio.t;
    typ : Type.t;
    name : string;
  }

  let of_c c =
    let flags = Ctypes.getf c flags in
    let stereo_mode = U32.logand flags CT.DrmModeModeInfo.Stereo_mode.mask in
    let aspect_ratio = U32.logand flags CT.DrmModeModeInfo.Aspect_ratio.mask in
    let ( -- ) = U32.sub in
    {
      clock = Ctypes.getf c clock;
      hdisplay = Ctypes.getf c hdisplay;
      hsync_start = Ctypes.getf c hsync_start;
      hsync_end = Ctypes.getf c hsync_end;
      htotal = Ctypes.getf c htotal;
      hskew = Ctypes.getf c hskew;
      vdisplay = Ctypes.getf c vdisplay;
      vsync_start = Ctypes.getf c vsync_start;
      vsync_end = Ctypes.getf c vsync_end;
      vtotal = Ctypes.getf c vtotal;
      vscan = Ctypes.getf c vscan;
      vrefresh = Ctypes.getf c vrefresh;
      flags = flags -- stereo_mode -- aspect_ratio;
      stereo_mode = Stereo_mode.of_c stereo_mode;
      aspect_ratio = Aspect_ratio.of_c aspect_ratio;
      typ = Ctypes.getf c typ;
      name = string_of_carray (Ctypes.getf c name);
    }

  let to_c t =
    let module T = CT.DrmModeModeInfo in
    let { clock; hdisplay; hsync_start; hsync_end; htotal; hskew; vdisplay;
          vsync_start; vsync_end; vtotal; vscan; vrefresh; flags; stereo_mode; aspect_ratio; typ; name } = t in
    let ptr = Ctypes.allocate_n T.t ~count:1 in
    let ( ++ ) = U32.logor in
    let c = !@ ptr in
    Ctypes.setf c T.clock clock;
    Ctypes.setf c T.hdisplay hdisplay;
    Ctypes.setf c T.hsync_start hsync_start;
    Ctypes.setf c T.hsync_end hsync_end;
    Ctypes.setf c T.htotal htotal;
    Ctypes.setf c T.hskew hskew;
    Ctypes.setf c T.vdisplay vdisplay;
    Ctypes.setf c T.vsync_start vsync_start;
    Ctypes.setf c T.vsync_end vsync_end;
    Ctypes.setf c T.vtotal vtotal;
    Ctypes.setf c T.vscan vscan;
    Ctypes.setf c T.vrefresh vrefresh;
    Ctypes.setf c T.flags (flags ++ Stereo_mode.to_c stereo_mode ++ Aspect_ratio.to_c aspect_ratio);
    Ctypes.setf c T.typ typ;
    Ctypes.setf c T.name (Ctypes.CArray.of_string name);
    ptr

  let vrefresh t =
    let num = t.clock in
    let den = t.htotal * t.vtotal in
    let num = if Flags.(mem interlace) t.flags then num * 2 else num in
    let den = if Flags.(mem dblscan) t.flags then den * 2 else den in
    let den = if t.vscan > 1 then den * t.vscan else den in
    Stdlib.float num *. 1000.00 /. Stdlib.float den

  let pp f t =
    Fmt.pf f "{@[<hv>name = %S;@ typ = @[<h>%a@];@ flags = @[<h>%a@];@ stereo_mode = %a;@ aspect_ratio = %a;@ clock = %d;@ hdisplay,vdisplay = %d,%d;@ hsync_start = %d;@ hsync_end = %d;@ htotal = %d;@ hskew = %d;@ vsync_start = %d;@ vsync_end = %d;@ vtotal = %d;@ vscan = %d;@ vrefresh = %d@]}"
      t.name
      Type.pp t.typ
      Flags.pp t.flags
      Stereo_mode.pp t.stereo_mode
      Aspect_ratio.pp t.aspect_ratio
      t.clock
      t.hdisplay t.vdisplay
      t.hsync_start t.hsync_end t.htotal t.hskew t.vsync_start t.vsync_end t.vtotal t.vscan t.vrefresh

  let pp_summary f t =
    Fmt.pf f "%s %.2fHz" t.name (vrefresh t)
end

module Resources = struct
  open CT.DrmModeRes

  type t = {
    fbs : [`Fb] Id.t list;
    crtcs : [`Crtc] Id.t list;
    connectors : [`Connector] Id.t list;
    encoders : [`Encoder] Id.t list;
    min_width : int;
    max_width : int;
    min_height : int;
    max_height : int;
  }

  let of_c c =
    let get f l = get_array c f l in
    {
      fbs = get fbs count_fbs;
      crtcs = get crtcs count_crtcs;
      connectors = get connectors count_connectors;
      encoders = get encoders count_encoders;
      min_width = Ctypes.getf c min_width;
      max_width = Ctypes.getf c max_width;
      min_height = Ctypes.getf c min_height;
      max_height = Ctypes.getf c max_height;
    }

  let pp f t =
    let ids f xs = Fmt.Dump.list Id.pp f xs in
    Fmt.pf f "{@[<hv>fbs = %a;@ crtcs = %a;@ connectors = %a;@ encoders = %a;@ min_width = %d;@ max_width = %d;@ min_height = %d;@ max_height = %d@]}"
      ids t.fbs
      ids t.crtcs
      ids t.connectors
      ids t.encoders
      t.min_width t.max_width t.min_height t.max_height

  let get fd =
    match C.Functions.drmModeGetResources fd with
    | None, errno -> Err.report errno "drmModeGetResources" ""
    | Some c, _ ->
      let x = of_c (!@ c) in
      C.Functions.drmModeFreeResources c |> Err.ignore;
      x
end

module Blob = struct
  open CT.DrmModePropertyBlob

  type id = [`Blob] Id.t

  type t = {
    id : id;
    data : string;
  }

  let of_c c =
    let ptr = Ctypes.getf c data |> Ctypes.(from_voidp char) in
    let length = Ctypes.getf c length in
    {
      id = Ctypes.getf c id;
      data = Ctypes.string_from_ptr ptr ~length;
    }

  let pp f t =
    Fmt.pf f "{@[<hv>id = %a;@ data = %S@]}"
      Id.pp t.id
      (if (String.length t.data < 10) then t.data else String.sub t.data 0 9 ^ "...")

  let get fd id =
    match C.Functions.drmModeGetPropertyBlob fd id with
    | Some c, _ ->
      let x = of_c (!@ c) in
      C.Functions.drmModeFreePropertyBlob c |> Err.ignore;
      Some x
    | None, errno ->
      match Err.code_of_errno errno with
      | ENOENT -> None
      | code -> raise (Unix.Unix_error (code, "drmModeFreePropertyBlob", ""))
end

module Property = struct
  type id = [`Property] Id.t
  type raw_value = U64.t

  module Info = struct

    module Named_value = struct
      open CT.DrmModePropertyEnum

      type t = {
        name : string;
        value : raw_value;
      }

      let of_c c = {
        value = Ctypes.getf c value;
        name = string_of_carray (Ctypes.getf c name);
      }

      let pp f t =
        Fmt.pf f "{@[<hv>value = %a;@ name = %S@]}"
          U64.pp t.value t.name

      let rec lookup x = function
        | [] -> None
        | { name; value } :: _ when x = value -> Some name
        | _ :: xs -> lookup x xs

      let rec lookup_name_exn x = function
        | [] -> Fmt.failwith "Enum value %S not known" x
        | { name; value } :: _ when x = name -> value
        | _ :: xs -> lookup_name_exn x xs
    end

    open CT.DrmModeProperty

    type ty =
      | Unsigned_range of int64 * int64
      | Signed_range of int64 * int64
      | Enum of Named_value.t list
      | Blob of Blob.id list
      | Bitmask of Named_value.t list
      | Object
      | Unknown of U32.t

    type t = {
      prop_id : id;
      name : string;
      ty : ty;
    }

    let of_c c =
      let ty, _errno = C.Functions.drmModeGetPropertyType c in
      let c = !@ c in
      let flags = Ctypes.getf c flags in
      let range fn =
        match get_array c values count_values with
        | [min; max] -> fn (U64.to_int64 min) (U64.to_int64 max)
        | _ -> Unknown flags
      in
      let ty =
        if ty = Flags.range then range (fun min max -> Unsigned_range (min, max))
        else if ty = Flags.enum then Enum (get_array c enums count_enums |> List.map Named_value.of_c)
        else if ty = Flags.blob then Blob (get_array c blob_ids count_blobs)
        else if ty = Flags.bitmask then Bitmask (get_array c enums count_enums |> List.map Named_value.of_c)
        else if ty = Flags.object_ then Object
        else if ty = Flags.signed_range then range (fun min max -> Signed_range (min, max))
        else Unknown flags
      in
      {
        prop_id = Ctypes.getf c prop_id;
        name = string_of_carray (Ctypes.getf c name);
        ty;
      }

    let pp_values f = function
      | Enum values -> Fmt.pf f "Enum %a" (Fmt.Dump.list Named_value.pp) values
      | Bitmask values -> Fmt.pf f "Bitmask %a" (Fmt.Dump.list Named_value.pp) values
      | Unsigned_range (min, max) -> Fmt.pf f "%Lu-%Lu" min max
      | Signed_range (min, max) -> Fmt.pf f "%Ld-%Ld" min max
      | Blob values -> Fmt.(Dump.list Id.pp) f values
      | Object -> Fmt.string f "Object"
      | Unknown x -> Fmt.pf f "Unknown %a" U32.pp x

    let pp f t =
      Fmt.pf f "{@[<hv>prop_id = %a;@ name = %S;@ values = %a@]}"
        Id.pp t.prop_id t.name pp_values t.ty

    let get fd id =
      match C.Functions.drmModeGetProperty fd id with
      | None, errno -> Err.report errno "drmModeGetProperty" ""
      | Some c, _ ->
        let x = of_c c in
        C.Functions.drmModeFreeProperty c |> Err.ignore;
        x
  end

  type ('obj, 'value) t = {
    name : string;
    read : Info.t -> U64.t -> 'value;
    write : Info.t -> 'value -> U64.t;
  }

  let create ~read ~write name =
    { name; read; write }

  let create_id_opt name =
    create ~read:(fun _ -> Id.of_uint64_opt) ~write:(fun _ -> Id.to_uint64_opt) name

  let create_enum name enum_values =
    let read (info : Info.t) x =
      match info.ty with
      | Enum values ->
        begin match Info.Named_value.lookup x values with
          | None -> `Unknown x
          | Some v_str ->
            match List.assoc_opt v_str enum_values with
            | Some v -> v
            | None -> `Unknown x
        end
      | _ -> `Unknown x
    in
    let write (info : Info.t) v =
      match List.find_opt (fun x -> snd x = v) enum_values with
      | None ->
        begin match v with
          | `Unknown x -> x
          | _ -> Fmt.failwith "Unexpected OCaml constructor for %S" info.name
        end
      | Some (v_str, _) ->
        match info.ty with
        | Enum values -> Info.Named_value.lookup_name_exn v_str values
        | _ -> Fmt.failwith "Property %S is not an enum!" info.name
    in
    create ~read ~write name
end

module Properties = struct
  module Type = struct
    open CT.DrmModeObjectType

    type _ t =
      | Crtc : [`Crtc] t
      | Connector : [`Connector] t
      | Encoder : [`Encoder] t
      | Mode : [`Mode] t
      | Property : [`Property] t
      | Fb : [`Fb] t
      | Blob : [`Blob] t
      | Plane : [`Plane] t
      | Any : _ t

    let to_c : type a. a t -> U32.t = function
      | Crtc -> crtc
      | Connector -> connector
      | Encoder -> encoder
      | Mode -> mode
      | Property -> property
      | Fb -> fb
      | Blob -> blob
      | Plane -> plane
      | Any -> any

    let to_string : type a. a t -> string = function
      | Crtc -> "CRTC"
      | Connector -> "Connector"
      | Encoder -> "Encoder"
      | Mode -> "Mode"
      | Property -> "Property"
      | Fb -> "FB"
      | Blob -> "Blob"
      | Plane -> "Plane"
      | Any -> "Any"

    let pp f x = Fmt.of_to_string to_string f x
  end

  open CT.DrmModeObjectProperties

  type value_info = {
    id : Property.id;
    value : Property.raw_value;
    info : Property.Info.t;
  }

  let pp_value f x =
    match x.info.ty with
    | Enum values ->
      begin match Property.Info.Named_value.lookup x.value values with
        | Some name -> Fmt.string f name
        | None -> U64.pp f x.value
      end
    | Bitmask values ->
      Fmt.(Dump.list string) f @@
      List.filter_map (fun (e : Property.Info.Named_value.t) ->
          let mask = U64.(shift_left one) (U64.to_int e.value) in
          if U64.logand x.value mask <> U64.zero then Some e.name else None
        ) values
    | Signed_range _ ->
      Fmt.int64 f (U64.to_int64 x.value)
    | Blob _ | Object ->
      if x.value = U64.zero then Fmt.string f "None"
      else U64.pp f x.value
    | Unsigned_range _ | Unknown _ ->
      U64.pp f x.value

  type 'a t = {
    id : 'a Id.t;
    props : value_info String_map.t;
  }

  let of_c c =
    let props = get_array c props count_props in
    let prop_values = get_array c prop_values count_props in
    List.map2 (fun k v -> (k, v)) props prop_values

  let pp f t =
    let pp_binding f (k, v) = Fmt.pf f "%s = %a" k pp_value v in
    Fmt.pf f "{@[<hov>%a@]}"
      (Fmt.iter_bindings String_map.iter pp_binding ~sep:Fmt.semi) t.props

  type binding = Property.id * Property.raw_value
  type raw_properties = binding list
  type object_types = [ `Blob | `Connector | `Crtc | `Encoder | `Fb | `Mode | `Plane | `Property ]

  let pp_binding f (k, v) =
    Fmt.pf f "%a:%a" Id.pp k U64.pp v

  let pp_raw = Fmt.Dump.list pp_binding

  let get_raw dev ty id =
    let id = (id : [< object_types] Id.t :> object_types Id.t) in
    match C.Functions.drmModeObjectGetProperties dev id (Type.to_c ty) with
    | None, errno -> Err.report errno "drmModeObjectGetProperties" ""
    | Some c, _ ->
      let x = of_c (!@ c) in
      C.Functions.drmModeFreeObjectProperties c |> Err.ignore;
      x

  let of_raw dev id raw =
    let props =
      let add idx (id, value) =
        let info = Property.Info.get dev id in
        let prop = {id; value; info } in
        String_map.add info.name prop idx
      in
      List.fold_left add String_map.empty raw
    in
    { id; props }

  let get dev ty id =
    of_raw dev id (get_raw dev ty id)

  let object_id t = t.id

  let get_value_info t (p : _ Property.t) =
    String_map.find_opt p.name t.props

  let get_value_info_exn t p =
    match get_value_info t p with
    | Some x -> x
    | None -> Fmt.failwith "No property %S on object %a" p.name Id.pp t.id

  let get_value t (prop : _ Property.t) =
    get_value_info t prop
    |> Option.map (fun x -> prop.read x.info x.value)

  let get_value_exn t prop =
    let x = get_value_info_exn t prop in
    prop.read x.info x.value

  let get_info t name =
    Option.map (fun x -> x.info) (String_map.find_opt name t.props)
end

module Crtc = struct
  open CT.DrmModeCrtc

  type id = [`Crtc] Id.t
  type 'a property = ([`Crtc], 'a) Property.t

  type t = {
    crtc_id : id;
    buffer_id : [`Fb] Id.t option;
    x : int;
    y : int;
    width : int;
    height : int;
    mode : Mode_info.t option;
    gamma_size : int;
  }

  let id t = t.crtc_id

  let of_c c =
    let mode_valid = Ctypes.getf c mode_valid <> 0 in
    {
      crtc_id = Ctypes.getf c crtc_id;
      buffer_id = Ctypes.getf c buffer_id;
      x = Ctypes.getf c x;
      y = Ctypes.getf c y;
      width = Ctypes.getf c width;
      height = Ctypes.getf c height;
      mode = if mode_valid then Some (Mode_info.of_c (Ctypes.getf c mode)) else None;
      gamma_size = Ctypes.getf c gamma_size;
    }

  let pp f t =
    Fmt.pf f "{@[<hv>crtc_id = %a;@ buffer_id = %a;@ x,y = (%d,%d);@ width,height = %dx%d;@ mode = %a@]}"
      Id.pp t.crtc_id (Fmt.Dump.option Id.pp) t.buffer_id t.x t.y t.width t.height (Fmt.Dump.option Mode_info.pp_summary) t.mode

  let get fd id =
    match C.Functions.drmModeGetCrtc fd id with
    | None, errno -> Err.report errno "drmModeGetCrtc" ""
    | Some c, _ ->
      let x = of_c (!@ c) in
      C.Functions.drmModeFreeCrtc c |> Err.ignore;
      x

  let set fd id ?buffer ~pos:(x, y) ~connectors mode =
    let connectors = Ctypes.(CArray.of_list C.Types.connector_id) connectors in
    let mode = Option.map Mode_info.to_c mode in
    match C.Functions.drmModeSetCrtc fd id buffer x y connectors.astart connectors.alength mode with
    | 0, _ -> ()
    | _, errno -> Err.report errno "drmModeSetCrtc" ""

  let page_flip ?(event=false) fd id ~user_data fb =
    let flags = if event then CT.PageFlipFlags.event else U32.zero in
    match C.Functions.drmModePageFlip fd id fb flags user_data with
    | 0, _ -> ()
    | _, errno -> Err.report errno "drmModePageFlip" ""

  let set_cursor fd id handle ~size:(width, height) =
    match C.Functions.drmModeSetCursor fd id handle width height with
    | 0, _ -> ()
    | _, errno -> Err.report errno "drmModeSetCursor" ""

  let move_cursor fd id (x, y) =
    match C.Functions.drmModeMoveCursor fd id x y with
    | 0, _ -> ()
    | _, errno -> Err.report errno "drmModeMoveCursor" ""

  let get_properties dev = Properties.get dev Crtc
end

module Sub_pixel = struct
  type t = CT.DrmModeSubPixel.t =
    | Unknown
    | Horizontal_rgb
    | Horizontal_bgr
    | Vertical_rgb
    | Vertical_bgr
    | None

  let pp f : t -> unit = function
    | Unknown -> Fmt.string f "Unknown"
    | Horizontal_rgb -> Fmt.string f "Horizontal_rgb"
    | Horizontal_bgr -> Fmt.string f "Horizontal_bgr"
    | Vertical_rgb -> Fmt.string f "Vertical_rgb"
    | Vertical_bgr -> Fmt.string f "Vertical_bgr"
    | None -> Fmt.string f "None"
end

module Encoder = struct
  module Type = struct
    type t =
      | NONE
      | DAC
      | TMDS
      | LVDS
      | TVDAC
      | VIRTUAL
      | DSI
      | DPMST
      | DPI
      | Unknown of U32.t

    let values =
      CT.DrmModeEncoderType.[
        v_None,    NONE;
        v_Dac,     DAC;
        v_Tmds,    TMDS;
        v_Lvds,    LVDS;
        v_Tvdac,   TVDAC;
        v_Virtual, VIRTUAL;
        v_Dsi,     DSI;
        v_Dpmst,   DPMST;
        v_Dpi,     DPI;
      ]

    let of_c c =
      List.assoc_opt c values |> Option.value ~default:(Unknown c)

    let to_string = function
      | NONE -> "NONE"
      | DAC -> "DAC"
      | TMDS -> "TMDS"
      | LVDS -> "LVDS"
      | TVDAC -> "TVDAC"
      | VIRTUAL -> "VIRTUAL"
      | DSI -> "DSI"
      | DPMST -> "DPMST"
      | DPI -> "DPI"
      | Unknown c -> U32.to_string c

    let pp = Fmt.of_to_string to_string
  end

  open CT.DrmModeEncoder

  type id = [`Encoder] Id.t

  type t = {
    encoder_id : id;
    encoder_type : Type.t;
    crtc_id : Crtc.id option;
    possible_crtcs : int;
    possible_clones : int;
  }

  let id t = t.encoder_id

  let of_c c = {
    encoder_id = Ctypes.getf c encoder_id;
    encoder_type = Type.of_c (Ctypes.getf c encoder_type);
    crtc_id = Ctypes.getf c crtc_id;
    possible_crtcs = U32.to_int (Ctypes.getf c possible_crtcs);
    possible_clones = U32.to_int (Ctypes.getf c possible_clones);
  }

  let pp f t =
    Fmt.pf f "{@[<hv>encoder_id = %a;@ encoder_type = %a;@ crtc_id = %a;@ possible_crtcs = %#x;@ possible_clones = %#x@]}"
      Id.pp t.encoder_id Type.pp t.encoder_type (Fmt.Dump.option Id.pp) t.crtc_id t.possible_crtcs t.possible_clones

  let get fd id =
    match C.Functions.drmModeGetEncoder fd id with
    | None, errno -> Err.report errno "drmModeGetEncoder" ""
    | Some c, _ ->
      let x = of_c (!@ c) in
      C.Functions.drmModeFreeEncoder c |> Err.ignore;
      x
end

module Connector = struct
  module Connection = struct
    type t = CT.DrmModeConnection.t =
      | Connected
      | Disconnected
      | Unknown_connection

    let pp f : t -> unit = function
      | Connected -> Fmt.string f "Connected"
      | Disconnected -> Fmt.string f "Disconnected"
      | Unknown_connection -> Fmt.string f "Unknown_connection"
  end

  module Type = struct
    type t =
      | Unknown
      | VGA
      | DVII
      | DVID
      | DVIA
      | Composite
      | SVIDEO
      | LVDS
      | Component
      | NinePinDIN
      | DisplayPort
      | HDMIA
      | HDMIB
      | TV
      | EDP
      | VIRTUAL
      | DSI
      | DPI
      | WRITEBACK
      | SPI
      | USB

    let values =
      CT.DrmModeConnectorType.[
        v_Unknown, Unknown;
        v_VGA, VGA;
        v_DVII, DVII;
        v_DVID, DVID;
        v_DVIA, DVIA;
        v_Composite, Composite;
        v_SVIDEO, SVIDEO;
        v_LVDS, LVDS;
        v_Component, Component;
        v_9PinDIN, NinePinDIN;
        v_DisplayPort, DisplayPort;
        v_HDMIA, HDMIA;
        v_HDMIB, HDMIB;
        v_TV, TV;
        v_eDP, EDP;
        v_VIRTUAL, VIRTUAL;
        v_DSI, DSI;
        v_DPI, DPI;
        v_WRITEBACK, WRITEBACK;
        v_SPI, SPI;
        v_USB, USB;
      ]

    let of_c c =
      List.assoc_opt c values |> Option.value ~default:Unknown

    let to_c t =
      fst (List.find (fun (_, constr) -> constr = t) values)

    let to_ocaml_name = function
      | Unknown -> "Unknown"
      | VGA -> "VGA"
      | DVII -> "DVII"
      | DVID -> "DVID"
      | DVIA -> "DVIA"
      | Composite -> "Composite"
      | SVIDEO -> "SVIDEO"
      | LVDS -> "LVDS"
      | Component -> "Component"
      | NinePinDIN -> "9PinDIN"
      | DisplayPort -> "DisplayPort"
      | HDMIA -> "HDMIA"
      | HDMIB -> "HDMIB"
      | TV -> "TV"
      | EDP -> "eDP"
      | VIRTUAL -> "VIRTUAL"
      | DSI -> "DSI"
      | DPI -> "DPI"
      | WRITEBACK -> "WRITEBACK"
      | SPI -> "SPI"
      | USB -> "USB"

    let name t =
      C.Functions.drmModeGetConnectorTypeName (to_c t)
      |> Err.ignore
      |> Ctypes.(coerce (ptr_opt char) string_opt)
      |> Option.value ~default:"Unknown"

    let pp = Fmt.of_to_string to_ocaml_name
  end

  open CT.DrmModeConnector

  type id = [`Connector] Id.t
  type 'a property = ([`Connector], 'a) Property.t

  type t = {
    connector_id : id;
    encoder_id : Encoder.id option;
    connector_type : Type.t;
    connector_type_id : int;
    connection : Connection.t;
    mm_width : int;
    mm_height : int;
    subpixel : Sub_pixel.t;
    modes : Mode_info.t list;
    props : Properties.raw_properties;
    encoders : Encoder.id list;
  }

  let id t = t.connector_id

  let of_c c =
    let props = get_array c props count_props in
    let prop_values = get_array c prop_values count_props in
    {
      connector_id = Ctypes.getf c connector_id;
      encoder_id = Ctypes.getf c encoder_id;
      connector_type = Type.of_c (Ctypes.getf c connector_type);
      connector_type_id = Ctypes.getf c connector_type_id;
      connection = Ctypes.getf c connection;
      mm_width = Ctypes.getf c mmWidth;
      mm_height = Ctypes.getf c mmHeight;
      subpixel = Ctypes.getf c subpixel;
      modes = get_array c modes count_modes |> List.map Mode_info.of_c;
      props = List.map2 (fun k v -> (k, v)) props prop_values;
      encoders = get_array c encoders count_encoders;
    }

  let pp_modes = Fmt.Dump.list Mode_info.pp_summary

  let pp_name f t = Fmt.pf f "%s-%d" (Type.name t.connector_type) t.connector_type_id

  let pp f t =
    Fmt.pf f "{@[<v>connector_id = %a; (* %a *)@ connector_type = %a;@ connector_type_id = %d;@ connection = %a;@ mmWidth = %d;@ mmHeight = %d;@ subpixel = %a;@ modes = %a;@ props = %a;@ encoder_id = %a;@ encoders = %a@]}"
      Id.pp t.connector_id pp_name t
      Type.pp t.connector_type t.connector_type_id Connection.pp t.connection
      t.mm_width t.mm_height
      Sub_pixel.pp t.subpixel
      (pp_limited 4 Mode_info.pp_summary) t.modes
      Properties.pp_raw t.props
      (Fmt.Dump.option Id.pp) t.encoder_id
      (Fmt.Dump.list Id.pp) t.encoders

  let get fd id =
    match C.Functions.drmModeGetConnector fd id with
    | None, errno -> Err.report errno "drmModeGetConnector" ""
    | Some c, _ ->
      let x = of_c (!@ c) in
      C.Functions.drmModeFreeConnector c |> Err.ignore;
      x

  let get_properties dev = Properties.get dev Connector
  let crtc_id = Property.create_id_opt "CRTC_ID"
end

module Plane = struct
  open CT.DrmModePlane

  type id = [`Plane] Id.t
  type 'a property = ([`Plane], 'a) Property.t

  type t = {
    formats : Fourcc.t list;
    plane_id : id;
    crtc_id : Crtc.id option;
    fb_id : [`Fb] Id.t option;
    crtc_x : int;
    crtc_y : int;
    x : int;
    y : int;
    possible_crtcs : int;
    (* gamma_size : int; https://dri.freedesktop.org/docs/drm/gpu/drm-uapi.html#c.drm_mode_get_plane says never used *)
  }

  let id t = t.plane_id

  let of_c c = {
    formats = get_array c formats count_formats;
    plane_id = Ctypes.getf c plane_id;
    crtc_id = Ctypes.getf c crtc_id;
    fb_id = Ctypes.getf c fb_id;
    crtc_x = Ctypes.getf c crtc_x;
    crtc_y = Ctypes.getf c crtc_y;
    x = Ctypes.getf c x;
    y = Ctypes.getf c y;
    possible_crtcs = U32.to_int (Ctypes.getf c possible_crtcs);
  }

  let pp f t =
    Fmt.pf f "{@[<hv>formats = %a;@ plane_id = %a;@ crtc_id = %a;@ fb_id = %a;@ crtc_x,crtc_y = (%d,%d);@ x,y = (%d,%d);@ possible_crtcs = %#x@]}"
      Fmt.(Dump.list Fourcc.pp) t.formats
      Id.pp t.plane_id
      Fmt.(Dump.option Id.pp) t.crtc_id
      Fmt.(Dump.option Id.pp) t.fb_id
      t.crtc_x t.crtc_y
      t.x t.y
      t.possible_crtcs

  let get fd id =
    match C.Functions.drmModeGetPlane fd id with
    | None, errno -> Err.report errno "drmModeGetPlane" ""
    | Some c, _ ->
      let x = of_c (!@ c) in
      C.Functions.drmModeFreePlane c |> Err.ignore;
      x

  let fb_id = Property.create_id_opt "FB_ID"

  let typ =
    Property.create_enum "type" [
      "Overlay", `Overlay;
      "Primary", `Primary;
      "Cursor", `Cursor;
    ]

  let crtc_id = Property.create_id_opt "CRTC_ID"

  let get_properties dev = Properties.get dev Plane
end

module Plane_resources = struct
  open CT.DrmModePlaneRes
  type t = Plane.id list

  let of_c c = get_array c planes count_planes

  let pp = Fmt.Dump.list Id.pp

  let get fd =
    match C.Functions.drmModeGetPlaneResources fd with
    | None, errno -> Err.report errno "drmModeGetPlaneResources" ""
    | Some c, _ ->
      let t = of_c (!@ c) in
      C.Functions.drmModeFreePlaneResources c |> Err.ignore;
      t
end

module Fb = struct
  module Plane = struct
    type 'handle t = {
      handle : 'handle;
      pitch : int;
      offset : int;
    }

    let pp f { handle; pitch; offset } =
      Fmt.pf f "{@[handle = %a;@ pitch = %d;@ offset = %d}"
        (Fmt.Dump.option Id.pp) handle
        pitch
        offset
  end

  open CT.DrmModeFB2

  type id = [`Fb] Id.t

  type t = {
    fb_id : id;
    width : int;
    height : int;
    pixel_format : Fourcc.t;
    modifier : Modifier.t option;
    interlaced : bool;
    planes : Buffer.id option Plane.t list;
  }

  let id t = t.fb_id

  let of_c c =
    let handles = Ctypes.getf c handles in
    let pitches = Ctypes.getf c pitches in
    let offsets = Ctypes.getf c offsets in
    let planes = List.init 4 (fun i ->
        let handle = Ctypes.CArray.get handles i in
        let pitch = Ctypes.CArray.get pitches i in
        let offset = Ctypes.CArray.get offsets i in
        { Plane.handle; pitch; offset }
      ) |> List.take_while (fun x -> x.Plane.pitch <> 0)
    in
    let flags = Ctypes.getf c flags in
    let modifier =
      if U32.logand flags CT.FbFlags.modifiers <> U32.zero then
        Some (Ctypes.getf c modifier)
      else
        None
    in
    {
      fb_id = Ctypes.getf c fb_id;
      width = Ctypes.getf c width;
      height = Ctypes.getf c height;
      pixel_format = Ctypes.getf c pixel_format;
      modifier;
      interlaced = (U32.logand flags CT.FbFlags.interlaced <> U32.zero);
      planes;
    }

  let pp f t =
    Fmt.pf f "{@[<hv>fb_id = %a;@ width,height = %dx%d;@ pixel_format, modifier = %a, %a;@ interlaced = %b;@ planes = %a@]}"
      Id.pp t.fb_id t.width t.height Fourcc.pp t.pixel_format (Fmt.Dump.option Modifier.pp) t.modifier t.interlaced
      (Fmt.Dump.list Plane.pp) t.planes

  let get fd id =
    match C.Functions.drmModeGetFB2 fd id with
    | None, errno -> Err.report errno "drmModeGetFB2" ""
    | Some c, _ ->
      let x = of_c (!@ c) in
      C.Functions.drmModeFreeFB2 c |> Err.ignore;
      x

  let add ?(interlaced=false) ?modifier fd ~size:(w, h) ~pixel_format ~planes =
    let handles = Ctypes.(CArray.make C.Types.buffer_id 4) in
    let pitches = Ctypes.(CArray.make C.Types.pitch 4) in
    let offsets = Ctypes.(CArray.make C.Types.offset 4) in
    let flags = if interlaced then CT.FbFlags.interlaced else U32.zero in
    let flags = if modifier = None then flags else U32.logor flags CT.FbFlags.modifiers in
    let modifier = Option.value modifier ~default:Modifier.linear in
    let modifiers = Ctypes.(CArray.make C.Types.drm_modifier 4 ~initial:modifier) in
    planes |> List.iteri (fun i { Plane.handle; pitch; offset } ->
        Ctypes.CArray.set handles i handle;
        Ctypes.CArray.set pitches i pitch;
        Ctypes.CArray.set offsets i offset;
      );
    let buf_id = Ctypes.(allocate_n C.Types.fb_id ~count:1) in
    match C.Functions.drmModeAddFB2WithModifiers fd w h pixel_format
            handles.astart pitches.astart offsets.astart modifiers.astart buf_id flags with
    | 0, _ -> !@ buf_id
    | _, errno -> Err.report errno "drmModeAddFB2WithModifiers" ""

  let close_plane_handles dev t =
    List.filter_map (fun (p : Buffer.id option Plane.t) -> p.handle) t.planes
    |> List.sort_uniq compare
    |> List.iter (Buffer.close dev)

  let rm fd id =
    match C.Functions.drmModeRmFB fd id with
    | 0, _ -> ()
    | _, errno -> Err.report errno "drmModeRmFB" ""
end

module Atomic_req = struct
  type t = C.Types.AtomicReqPtr.t Ctypes.ptr

  let create () =
    match C.Functions.drmModeAtomicAlloc () with
    | None, errno -> Err.report errno "drmModeAtomicAlloc" ""
    | Some t, _ ->
      Gc.finalise (fun t -> C.Functions.drmModeAtomicFree t |> Err.ignore) t;
      t

  let add_property_full t (obj : _ Properties.t) (property : _ Property.t) value =
    let obj_id = (obj.id : [< Properties.object_types] Id.t :> Properties.object_types Id.t) in
    let info = Properties.get_value_info_exn obj property in
    let cursor = C.Functions.drmModeAtomicAddProperty t obj_id info.id (property.write info.info value) |> Err.ignore in
    if cursor < 0 then Err.report_neg cursor "drmModeAtomicAddProperty" ""
    else cursor

  let add_property t obj property value =
    ignore (add_property_full t obj property value : int)

  let flag b code x =
    if b then U32.logor x code
    else x

  let commit
      ?(page_flip_event=false)
      ?(page_flip_async=false)
      ?(test_only= false)
      ?(nonblock=false)
      ?(allow_modeset=false)
      dev t =
    let flags =
      U32.zero
      |> flag page_flip_event CT.PageFlipFlags.event
      |> flag page_flip_async CT.PageFlipFlags.async
      |> flag test_only CT.AtomicFlags.test_only
      |> flag nonblock CT.AtomicFlags.nonblock
      |> flag allow_modeset CT.AtomicFlags.allow_modeset
    in
    let user_data = Ctypes.null in
    match C.Functions.drmModeAtomicCommit dev t flags user_data with
    | 0, _ -> ()
    | _, errno -> Err.report errno "drmModeAtomicCommit" ""
end
