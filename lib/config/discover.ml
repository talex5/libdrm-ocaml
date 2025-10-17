module C = Configurator.V1

let int_constants = [
  "DRM_NODE_MAX";
  "DRM_PLATFORM_DEVICE_NAME_LEN";
  "DRM_HOST1X_DEVICE_NAME_LEN";
  "DRM_DISPLAY_MODE_LEN";
  "DRM_PROP_NAME_LEN";

  "DRM_MODE_CONNECTED";
  "DRM_MODE_DISCONNECTED";
  "DRM_MODE_UNKNOWNCONNECTION";

  "DRM_MODE_SUBPIXEL_UNKNOWN";
  "DRM_MODE_SUBPIXEL_HORIZONTAL_RGB";
  "DRM_MODE_SUBPIXEL_HORIZONTAL_BGR";
  "DRM_MODE_SUBPIXEL_VERTICAL_RGB";
  "DRM_MODE_SUBPIXEL_VERTICAL_BGR";
  "DRM_MODE_SUBPIXEL_NONE";
]

let sizes = [
  "drmModeConnection";
  "drmModeSubPixel";
]

let () =
  C.main ~name:"checkdrm" (fun c ->
      let conf =
        match C.Pkg_config.get c with
        | None -> failwith "pkg-config is not installed"
        | Some pc ->
          match C.Pkg_config.query pc ~package:"libdrm" with
          | None -> failwith "libdrm is not installed (according to pkg-config)"
          | Some deps -> deps
      in
      let defs =
        C.C_define.import c ~c_flags:conf.cflags ~includes:["linux/dma-buf.h"; "xf86drm.h"; "xf86drmMode.h"]
          (List.map (fun c -> c, C.C_define.Type.Int) int_constants)
        |> List.map (function
            | name, C.C_define.Value.Int v ->
              Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      let sizes =
        C.C_define.import c ~c_flags:conf.cflags ~includes:["linux/dma-buf.h"; "xf86drm.h"; "xf86drmMode.h"]
          (List.map (fun c -> Printf.sprintf "sizeof(%s)" c, C.C_define.Type.Int) sizes)
        |> List.map (function
            | expr, C.C_define.Value.Int v ->
              let name = String.sub expr 7 (String.length expr - 8) in
              Printf.sprintf "let sizeof_%s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      C.Flags.write_lines "config.ml" (defs @ sizes);
      C.Flags.write_sexp "c_flags.sexp"         conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs
    )
