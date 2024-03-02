open Aws.BaseTypes
type calendar = CalendarLib.Calendar.t
module StringSetAttributeValue =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module NumberSetAttributeValue =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module BinarySetAttributeValue =
  struct
    type t = Blob.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Blob.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Blob.to_query v
    let to_json v = `List (List.map Blob.to_json v)
    let of_json j = Aws.Json.to_list Blob.of_json j
  end
module rec
  MapAttributeValue:sig
                      type t = (String.t, AttributeValue.t) Hashtbl.t
                      val make : 'a -> unit -> 'a
                      val parse : Ezxmlm.nodes -> t option
                      val to_query : t -> Aws.Query.t
                      val to_json : t -> Aws.Json.t
                      val of_json : Aws.Json.t -> t
                    end =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
 and
  ListAttributeValue:sig
                       type t = AttributeValue.t list
                       val make : 'a -> unit -> 'a
                       val parse : Ezxmlm.nodes -> t option
                       val to_query : t -> Aws.Query.t
                       val to_json : t -> Aws.Json.t
                       val of_json : Aws.Json.t -> t
                     end =
  struct
    type t = AttributeValue.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map AttributeValue.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list AttributeValue.to_query v
    let to_json v = `List (List.map AttributeValue.to_json v)
    let of_json j = Aws.Json.to_list AttributeValue.of_json j
  end and
       AttributeValue:sig
                        type t =
                          {
                          s: String.t option ;
                          n: String.t option ;
                          b: Blob.t option ;
                          s_s: StringSetAttributeValue.t ;
                          n_s: NumberSetAttributeValue.t ;
                          b_s: BinarySetAttributeValue.t ;
                          m: MapAttributeValue.t option ;
                          l: ListAttributeValue.t ;
                          n_u_l_l: Boolean.t option ;
                          b_o_o_l: Boolean.t option }
                        val make :
                          ?s:String.t ->
                            ?n:String.t ->
                              ?b:Blob.t ->
                                ?s_s:StringSetAttributeValue.t ->
                                  ?n_s:NumberSetAttributeValue.t ->
                                    ?b_s:BinarySetAttributeValue.t ->
                                      ?m:MapAttributeValue.t ->
                                        ?l:ListAttributeValue.t ->
                                          ?n_u_l_l:Boolean.t ->
                                            ?b_o_o_l:Boolean.t -> unit -> t
                        val parse : Ezxmlm.nodes -> t option
                        val to_query : t -> Aws.Query.t
                        val to_json : t -> Aws.Json.t
                        val of_json : Aws.Json.t -> t
                      end =
       struct
         type t =
           {
           s: String.t option ;
           n: String.t option ;
           b: Blob.t option ;
           s_s: StringSetAttributeValue.t ;
           n_s: NumberSetAttributeValue.t ;
           b_s: BinarySetAttributeValue.t ;
           m: MapAttributeValue.t option ;
           l: ListAttributeValue.t ;
           n_u_l_l: Boolean.t option ;
           b_o_o_l: Boolean.t option }
         let make ?s  ?n  ?b  ?(s_s= [])  ?(n_s= [])  ?(b_s= [])  ?m  ?(l=
           [])  ?n_u_l_l  ?b_o_o_l  () =
           { s; n; b; s_s; n_s; b_s; m; l; n_u_l_l; b_o_o_l }
         let parse xml =
           Some
             {
               s =
                 (Aws.Util.option_bind (Aws.Xml.member "S" xml) String.parse);
               n =
                 (Aws.Util.option_bind (Aws.Xml.member "N" xml) String.parse);
               b = (Aws.Util.option_bind (Aws.Xml.member "B" xml) Blob.parse);
               s_s =
                 (Aws.Util.of_option []
                    (Aws.Util.option_bind (Aws.Xml.member "SS" xml)
                       StringSetAttributeValue.parse));
               n_s =
                 (Aws.Util.of_option []
                    (Aws.Util.option_bind (Aws.Xml.member "NS" xml)
                       NumberSetAttributeValue.parse));
               b_s =
                 (Aws.Util.of_option []
                    (Aws.Util.option_bind (Aws.Xml.member "BS" xml)
                       BinarySetAttributeValue.parse));
               m =
                 (Aws.Util.option_bind (Aws.Xml.member "M" xml)
                    MapAttributeValue.parse);
               l =
                 (Aws.Util.of_option []
                    (Aws.Util.option_bind (Aws.Xml.member "L" xml)
                       ListAttributeValue.parse));
               n_u_l_l =
                 (Aws.Util.option_bind (Aws.Xml.member "NULL" xml)
                    Boolean.parse);
               b_o_o_l =
                 (Aws.Util.option_bind (Aws.Xml.member "BOOL" xml)
                    Boolean.parse)
             }
         let to_query v =
           Aws.Query.List
             (Aws.Util.list_filter_opt
                [Aws.Util.option_map v.b_o_o_l
                   (fun f -> Aws.Query.Pair ("BOOL", (Boolean.to_query f)));
                Aws.Util.option_map v.n_u_l_l
                  (fun f -> Aws.Query.Pair ("NULL", (Boolean.to_query f)));
                Some
                  (Aws.Query.Pair
                     ("L.member", (ListAttributeValue.to_query v.l)));
                Aws.Util.option_map v.m
                  (fun f ->
                     Aws.Query.Pair ("M", (MapAttributeValue.to_query f)));
                Some
                  (Aws.Query.Pair
                     ("BS.member", (BinarySetAttributeValue.to_query v.b_s)));
                Some
                  (Aws.Query.Pair
                     ("NS.member", (NumberSetAttributeValue.to_query v.n_s)));
                Some
                  (Aws.Query.Pair
                     ("SS.member", (StringSetAttributeValue.to_query v.s_s)));
                Aws.Util.option_map v.b
                  (fun f -> Aws.Query.Pair ("B", (Blob.to_query f)));
                Aws.Util.option_map v.n
                  (fun f -> Aws.Query.Pair ("N", (String.to_query f)));
                Aws.Util.option_map v.s
                  (fun f -> Aws.Query.Pair ("S", (String.to_query f)))])
         let to_json v =
           `Assoc
             (Aws.Util.list_filter_opt
                [Aws.Util.option_map v.b_o_o_l
                   (fun f -> ("BOOL", (Boolean.to_json f)));
                Aws.Util.option_map v.n_u_l_l
                  (fun f -> ("NULL", (Boolean.to_json f)));
                Some ("L", (ListAttributeValue.to_json v.l));
                Aws.Util.option_map v.m
                  (fun f -> ("M", (MapAttributeValue.to_json f)));
                Some ("BS", (BinarySetAttributeValue.to_json v.b_s));
                Some ("NS", (NumberSetAttributeValue.to_json v.n_s));
                Some ("SS", (StringSetAttributeValue.to_json v.s_s));
                Aws.Util.option_map v.b (fun f -> ("B", (Blob.to_json f)));
                Aws.Util.option_map v.n (fun f -> ("N", (String.to_json f)));
                Aws.Util.option_map v.s (fun f -> ("S", (String.to_json f)))])
         let of_json j =
           {
             s = (Aws.Util.option_map (Aws.Json.lookup j "S") String.of_json);
             n = (Aws.Util.option_map (Aws.Json.lookup j "N") String.of_json);
             b = (Aws.Util.option_map (Aws.Json.lookup j "B") Blob.of_json);
             s_s =
               (StringSetAttributeValue.of_json
                  (Aws.Util.of_option_exn (Aws.Json.lookup j "SS")));
             n_s =
               (NumberSetAttributeValue.of_json
                  (Aws.Util.of_option_exn (Aws.Json.lookup j "NS")));
             b_s =
               (BinarySetAttributeValue.of_json
                  (Aws.Util.of_option_exn (Aws.Json.lookup j "BS")));
             m =
               (Aws.Util.option_map (Aws.Json.lookup j "M")
                  MapAttributeValue.of_json);
             l =
               (ListAttributeValue.of_json
                  (Aws.Util.of_option_exn (Aws.Json.lookup j "L")));
             n_u_l_l =
               (Aws.Util.option_map (Aws.Json.lookup j "NULL")
                  Boolean.of_json);
             b_o_o_l =
               (Aws.Util.option_map (Aws.Json.lookup j "BOOL")
                  Boolean.of_json)
           }
       end
module StreamViewType =
  struct
    type t =
      | NEW_IMAGE 
      | OLD_IMAGE 
      | NEW_AND_OLD_IMAGES 
      | KEYS_ONLY 
    let str_to_t =
      [("KEYS_ONLY", KEYS_ONLY);
      ("NEW_AND_OLD_IMAGES", NEW_AND_OLD_IMAGES);
      ("OLD_IMAGE", OLD_IMAGE);
      ("NEW_IMAGE", NEW_IMAGE)]
    let t_to_str =
      [(KEYS_ONLY, "KEYS_ONLY");
      (NEW_AND_OLD_IMAGES, "NEW_AND_OLD_IMAGES");
      (OLD_IMAGE, "OLD_IMAGE");
      (NEW_IMAGE, "NEW_IMAGE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module StreamSpecification =
  struct
    type t =
      {
      stream_enabled: Boolean.t option ;
      stream_view_type: StreamViewType.t option }
    let make ?stream_enabled  ?stream_view_type  () =
      { stream_enabled; stream_view_type }
    let parse xml =
      Some
        {
          stream_enabled =
            (Aws.Util.option_bind (Aws.Xml.member "StreamEnabled" xml)
               Boolean.parse);
          stream_view_type =
            (Aws.Util.option_bind (Aws.Xml.member "StreamViewType" xml)
               StreamViewType.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.stream_view_type
              (fun f ->
                 Aws.Query.Pair
                   ("StreamViewType", (StreamViewType.to_query f)));
           Aws.Util.option_map v.stream_enabled
             (fun f -> Aws.Query.Pair ("StreamEnabled", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.stream_view_type
              (fun f -> ("StreamViewType", (StreamViewType.to_json f)));
           Aws.Util.option_map v.stream_enabled
             (fun f -> ("StreamEnabled", (Boolean.to_json f)))])
    let of_json j =
      {
        stream_enabled =
          (Aws.Util.option_map (Aws.Json.lookup j "StreamEnabled")
             Boolean.of_json);
        stream_view_type =
          (Aws.Util.option_map (Aws.Json.lookup j "StreamViewType")
             StreamViewType.of_json)
      }
  end
module ProvisionedThroughput =
  struct
    type t = {
      read_capacity_units: Long.t ;
      write_capacity_units: Long.t }
    let make ~read_capacity_units  ~write_capacity_units  () =
      { read_capacity_units; write_capacity_units }
    let parse xml =
      Some
        {
          read_capacity_units =
            (Aws.Xml.required "ReadCapacityUnits"
               (Aws.Util.option_bind (Aws.Xml.member "ReadCapacityUnits" xml)
                  Long.parse));
          write_capacity_units =
            (Aws.Xml.required "WriteCapacityUnits"
               (Aws.Util.option_bind
                  (Aws.Xml.member "WriteCapacityUnits" xml) Long.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("WriteCapacityUnits",
                   (Long.to_query v.write_capacity_units)));
           Some
             (Aws.Query.Pair
                ("ReadCapacityUnits", (Long.to_query v.read_capacity_units)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("WriteCapacityUnits", (Long.to_json v.write_capacity_units));
           Some ("ReadCapacityUnits", (Long.to_json v.read_capacity_units))])
    let of_json j =
      {
        read_capacity_units =
          (Long.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ReadCapacityUnits")));
        write_capacity_units =
          (Long.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "WriteCapacityUnits")))
      }
  end
module UpdateGlobalSecondaryIndexAction =
  struct
    type t =
      {
      index_name: String.t ;
      provisioned_throughput: ProvisionedThroughput.t }
    let make ~index_name  ~provisioned_throughput  () =
      { index_name; provisioned_throughput }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Aws.Util.option_bind (Aws.Xml.member "IndexName" xml)
                  String.parse));
          provisioned_throughput =
            (Aws.Xml.required "ProvisionedThroughput"
               (Aws.Util.option_bind
                  (Aws.Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("ProvisionedThroughput",
                   (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("ProvisionedThroughput",
                (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ProvisionedThroughput")))
      }
  end
module DeleteGlobalSecondaryIndexAction =
  struct
    type t = {
      index_name: String.t }
    let make ~index_name  () = { index_name }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Aws.Util.option_bind (Aws.Xml.member "IndexName" xml)
                  String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")))
      }
  end
module ProjectionType =
  struct
    type t =
      | ALL 
      | KEYS_ONLY 
      | INCLUDE 
    let str_to_t =
      [("INCLUDE", INCLUDE); ("KEYS_ONLY", KEYS_ONLY); ("ALL", ALL)]
    let t_to_str =
      [(INCLUDE, "INCLUDE"); (KEYS_ONLY, "KEYS_ONLY"); (ALL, "ALL")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module NonKeyAttributeNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module Projection =
  struct
    type t =
      {
      projection_type: ProjectionType.t option ;
      non_key_attributes: NonKeyAttributeNameList.t }
    let make ?projection_type  ?(non_key_attributes= [])  () =
      { projection_type; non_key_attributes }
    let parse xml =
      Some
        {
          projection_type =
            (Aws.Util.option_bind (Aws.Xml.member "ProjectionType" xml)
               ProjectionType.parse);
          non_key_attributes =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "NonKeyAttributes" xml)
                  NonKeyAttributeNameList.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("NonKeyAttributes.member",
                   (NonKeyAttributeNameList.to_query v.non_key_attributes)));
           Aws.Util.option_map v.projection_type
             (fun f ->
                Aws.Query.Pair
                  ("ProjectionType", (ProjectionType.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("NonKeyAttributes",
                (NonKeyAttributeNameList.to_json v.non_key_attributes));
           Aws.Util.option_map v.projection_type
             (fun f -> ("ProjectionType", (ProjectionType.to_json f)))])
    let of_json j =
      {
        projection_type =
          (Aws.Util.option_map (Aws.Json.lookup j "ProjectionType")
             ProjectionType.of_json);
        non_key_attributes =
          (NonKeyAttributeNameList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "NonKeyAttributes")))
      }
  end
module KeyType =
  struct
    type t =
      | HASH 
      | RANGE 
    let str_to_t = [("RANGE", RANGE); ("HASH", HASH)]
    let t_to_str = [(RANGE, "RANGE"); (HASH, "HASH")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module KeySchemaElement =
  struct
    type t = {
      attribute_name: String.t ;
      key_type: KeyType.t }
    let make ~attribute_name  ~key_type  () = { attribute_name; key_type }
    let parse xml =
      Some
        {
          attribute_name =
            (Aws.Xml.required "AttributeName"
               (Aws.Util.option_bind (Aws.Xml.member "AttributeName" xml)
                  String.parse));
          key_type =
            (Aws.Xml.required "KeyType"
               (Aws.Util.option_bind (Aws.Xml.member "KeyType" xml)
                  KeyType.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("KeyType", (KeyType.to_query v.key_type)));
           Some
             (Aws.Query.Pair
                ("AttributeName", (String.to_query v.attribute_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("KeyType", (KeyType.to_json v.key_type));
           Some ("AttributeName", (String.to_json v.attribute_name))])
    let of_json j =
      {
        attribute_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeName")));
        key_type =
          (KeyType.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyType")))
      }
  end
module KeySchema =
  struct
    type t = KeySchemaElement.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map KeySchemaElement.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list KeySchemaElement.to_query v
    let to_json v = `List (List.map KeySchemaElement.to_json v)
    let of_json j = Aws.Json.to_list KeySchemaElement.of_json j
  end
module CreateGlobalSecondaryIndexAction =
  struct
    type t =
      {
      index_name: String.t ;
      key_schema: KeySchema.t ;
      projection: Projection.t ;
      provisioned_throughput: ProvisionedThroughput.t }
    let make ~index_name  ~key_schema  ~projection  ~provisioned_throughput 
      () = { index_name; key_schema; projection; provisioned_throughput }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Aws.Util.option_bind (Aws.Xml.member "IndexName" xml)
                  String.parse));
          key_schema =
            (Aws.Xml.required "KeySchema"
               (Aws.Util.option_bind (Aws.Xml.member "KeySchema" xml)
                  KeySchema.parse));
          projection =
            (Aws.Xml.required "Projection"
               (Aws.Util.option_bind (Aws.Xml.member "Projection" xml)
                  Projection.parse));
          provisioned_throughput =
            (Aws.Xml.required "ProvisionedThroughput"
               (Aws.Util.option_bind
                  (Aws.Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("ProvisionedThroughput",
                   (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Aws.Query.Pair
                ("Projection", (Projection.to_query v.projection)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some
             (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("ProvisionedThroughput",
                (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some ("Projection", (Projection.to_json v.projection));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")));
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        projection =
          (Projection.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Projection")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ProvisionedThroughput")))
      }
  end
module GlobalSecondaryIndexUpdate =
  struct
    type t =
      {
      update: UpdateGlobalSecondaryIndexAction.t option ;
      create: CreateGlobalSecondaryIndexAction.t option ;
      delete: DeleteGlobalSecondaryIndexAction.t option }
    let make ?update  ?create  ?delete  () = { update; create; delete }
    let parse xml =
      Some
        {
          update =
            (Aws.Util.option_bind (Aws.Xml.member "Update" xml)
               UpdateGlobalSecondaryIndexAction.parse);
          create =
            (Aws.Util.option_bind (Aws.Xml.member "Create" xml)
               CreateGlobalSecondaryIndexAction.parse);
          delete =
            (Aws.Util.option_bind (Aws.Xml.member "Delete" xml)
               DeleteGlobalSecondaryIndexAction.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.delete
              (fun f ->
                 Aws.Query.Pair
                   ("Delete", (DeleteGlobalSecondaryIndexAction.to_query f)));
           Aws.Util.option_map v.create
             (fun f ->
                Aws.Query.Pair
                  ("Create", (CreateGlobalSecondaryIndexAction.to_query f)));
           Aws.Util.option_map v.update
             (fun f ->
                Aws.Query.Pair
                  ("Update", (UpdateGlobalSecondaryIndexAction.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.delete
              (fun f ->
                 ("Delete", (DeleteGlobalSecondaryIndexAction.to_json f)));
           Aws.Util.option_map v.create
             (fun f ->
                ("Create", (CreateGlobalSecondaryIndexAction.to_json f)));
           Aws.Util.option_map v.update
             (fun f ->
                ("Update", (UpdateGlobalSecondaryIndexAction.to_json f)))])
    let of_json j =
      {
        update =
          (Aws.Util.option_map (Aws.Json.lookup j "Update")
             UpdateGlobalSecondaryIndexAction.of_json);
        create =
          (Aws.Util.option_map (Aws.Json.lookup j "Create")
             CreateGlobalSecondaryIndexAction.of_json);
        delete =
          (Aws.Util.option_map (Aws.Json.lookup j "Delete")
             DeleteGlobalSecondaryIndexAction.of_json)
      }
  end
module GlobalSecondaryIndexUpdateList =
  struct
    type t = GlobalSecondaryIndexUpdate.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map GlobalSecondaryIndexUpdate.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list GlobalSecondaryIndexUpdate.to_query v
    let to_json v = `List (List.map GlobalSecondaryIndexUpdate.to_json v)
    let of_json j = Aws.Json.to_list GlobalSecondaryIndexUpdate.of_json j
  end
module ScalarAttributeType =
  struct
    type t =
      | S 
      | N 
      | B 
    let str_to_t = [("B", B); ("N", N); ("S", S)]
    let t_to_str = [(B, "B"); (N, "N"); (S, "S")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AttributeDefinition =
  struct
    type t =
      {
      attribute_name: String.t ;
      attribute_type: ScalarAttributeType.t }
    let make ~attribute_name  ~attribute_type  () =
      { attribute_name; attribute_type }
    let parse xml =
      Some
        {
          attribute_name =
            (Aws.Xml.required "AttributeName"
               (Aws.Util.option_bind (Aws.Xml.member "AttributeName" xml)
                  String.parse));
          attribute_type =
            (Aws.Xml.required "AttributeType"
               (Aws.Util.option_bind (Aws.Xml.member "AttributeType" xml)
                  ScalarAttributeType.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("AttributeType",
                   (ScalarAttributeType.to_query v.attribute_type)));
           Some
             (Aws.Query.Pair
                ("AttributeName", (String.to_query v.attribute_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("AttributeType",
                (ScalarAttributeType.to_json v.attribute_type));
           Some ("AttributeName", (String.to_json v.attribute_name))])
    let of_json j =
      {
        attribute_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeName")));
        attribute_type =
          (ScalarAttributeType.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeType")))
      }
  end
module AttributeDefinitions =
  struct
    type t = AttributeDefinition.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map AttributeDefinition.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list AttributeDefinition.to_query v
    let to_json v = `List (List.map AttributeDefinition.to_json v)
    let of_json j = Aws.Json.to_list AttributeDefinition.of_json j
  end
module UpdateTableInput =
  struct
    type t =
      {
      attribute_definitions: AttributeDefinitions.t ;
      table_name: String.t ;
      provisioned_throughput: ProvisionedThroughput.t option ;
      global_secondary_index_updates: GlobalSecondaryIndexUpdateList.t ;
      stream_specification: StreamSpecification.t option }
    let make ?(attribute_definitions= [])  ~table_name 
      ?provisioned_throughput  ?(global_secondary_index_updates= []) 
      ?stream_specification  () =
      {
        attribute_definitions;
        table_name;
        provisioned_throughput;
        global_secondary_index_updates;
        stream_specification
      }
    let parse xml =
      Some
        {
          attribute_definitions =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "AttributeDefinitions" xml)
                  AttributeDefinitions.parse));
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse));
          provisioned_throughput =
            (Aws.Util.option_bind
               (Aws.Xml.member "ProvisionedThroughput" xml)
               ProvisionedThroughput.parse);
          global_secondary_index_updates =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "GlobalSecondaryIndexUpdates" xml)
                  GlobalSecondaryIndexUpdateList.parse));
          stream_specification =
            (Aws.Util.option_bind (Aws.Xml.member "StreamSpecification" xml)
               StreamSpecification.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.stream_specification
              (fun f ->
                 Aws.Query.Pair
                   ("StreamSpecification", (StreamSpecification.to_query f)));
           Some
             (Aws.Query.Pair
                ("GlobalSecondaryIndexUpdates.member",
                  (GlobalSecondaryIndexUpdateList.to_query
                     v.global_secondary_index_updates)));
           Aws.Util.option_map v.provisioned_throughput
             (fun f ->
                Aws.Query.Pair
                  ("ProvisionedThroughput",
                    (ProvisionedThroughput.to_query f)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)));
           Some
             (Aws.Query.Pair
                ("AttributeDefinitions.member",
                  (AttributeDefinitions.to_query v.attribute_definitions)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.stream_specification
              (fun f ->
                 ("StreamSpecification", (StreamSpecification.to_json f)));
           Some
             ("GlobalSecondaryIndexUpdates",
               (GlobalSecondaryIndexUpdateList.to_json
                  v.global_secondary_index_updates));
           Aws.Util.option_map v.provisioned_throughput
             (fun f ->
                ("ProvisionedThroughput", (ProvisionedThroughput.to_json f)));
           Some ("TableName", (String.to_json v.table_name));
           Some
             ("AttributeDefinitions",
               (AttributeDefinitions.to_json v.attribute_definitions))])
    let of_json j =
      {
        attribute_definitions =
          (AttributeDefinitions.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "AttributeDefinitions")));
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        provisioned_throughput =
          (Aws.Util.option_map (Aws.Json.lookup j "ProvisionedThroughput")
             ProvisionedThroughput.of_json);
        global_secondary_index_updates =
          (GlobalSecondaryIndexUpdateList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "GlobalSecondaryIndexUpdates")));
        stream_specification =
          (Aws.Util.option_map (Aws.Json.lookup j "StreamSpecification")
             StreamSpecification.of_json)
      }
  end
module ItemCollectionSizeEstimateRange =
  struct
    type t = Double.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Double.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Double.to_query v
    let to_json v = `List (List.map Double.to_json v)
    let of_json j = Aws.Json.to_list Double.of_json j
  end
module TableStatus =
  struct
    type t =
      | CREATING 
      | UPDATING 
      | DELETING 
      | ACTIVE 
    let str_to_t =
      [("ACTIVE", ACTIVE);
      ("DELETING", DELETING);
      ("UPDATING", UPDATING);
      ("CREATING", CREATING)]
    let t_to_str =
      [(ACTIVE, "ACTIVE");
      (DELETING, "DELETING");
      (UPDATING, "UPDATING");
      (CREATING, "CREATING")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ProvisionedThroughputDescription =
  struct
    type t =
      {
      last_increase_date_time: DateTime.t option ;
      last_decrease_date_time: DateTime.t option ;
      number_of_decreases_today: Long.t option ;
      read_capacity_units: Long.t option ;
      write_capacity_units: Long.t option }
    let make ?last_increase_date_time  ?last_decrease_date_time 
      ?number_of_decreases_today  ?read_capacity_units  ?write_capacity_units
       () =
      {
        last_increase_date_time;
        last_decrease_date_time;
        number_of_decreases_today;
        read_capacity_units;
        write_capacity_units
      }
    let parse xml =
      Some
        {
          last_increase_date_time =
            (Aws.Util.option_bind (Aws.Xml.member "LastIncreaseDateTime" xml)
               DateTime.parse);
          last_decrease_date_time =
            (Aws.Util.option_bind (Aws.Xml.member "LastDecreaseDateTime" xml)
               DateTime.parse);
          number_of_decreases_today =
            (Aws.Util.option_bind
               (Aws.Xml.member "NumberOfDecreasesToday" xml) Long.parse);
          read_capacity_units =
            (Aws.Util.option_bind (Aws.Xml.member "ReadCapacityUnits" xml)
               Long.parse);
          write_capacity_units =
            (Aws.Util.option_bind (Aws.Xml.member "WriteCapacityUnits" xml)
               Long.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.write_capacity_units
              (fun f ->
                 Aws.Query.Pair ("WriteCapacityUnits", (Long.to_query f)));
           Aws.Util.option_map v.read_capacity_units
             (fun f ->
                Aws.Query.Pair ("ReadCapacityUnits", (Long.to_query f)));
           Aws.Util.option_map v.number_of_decreases_today
             (fun f ->
                Aws.Query.Pair ("NumberOfDecreasesToday", (Long.to_query f)));
           Aws.Util.option_map v.last_decrease_date_time
             (fun f ->
                Aws.Query.Pair
                  ("LastDecreaseDateTime", (DateTime.to_query f)));
           Aws.Util.option_map v.last_increase_date_time
             (fun f ->
                Aws.Query.Pair
                  ("LastIncreaseDateTime", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.write_capacity_units
              (fun f -> ("WriteCapacityUnits", (Long.to_json f)));
           Aws.Util.option_map v.read_capacity_units
             (fun f -> ("ReadCapacityUnits", (Long.to_json f)));
           Aws.Util.option_map v.number_of_decreases_today
             (fun f -> ("NumberOfDecreasesToday", (Long.to_json f)));
           Aws.Util.option_map v.last_decrease_date_time
             (fun f -> ("LastDecreaseDateTime", (DateTime.to_json f)));
           Aws.Util.option_map v.last_increase_date_time
             (fun f -> ("LastIncreaseDateTime", (DateTime.to_json f)))])
    let of_json j =
      {
        last_increase_date_time =
          (Aws.Util.option_map (Aws.Json.lookup j "LastIncreaseDateTime")
             DateTime.of_json);
        last_decrease_date_time =
          (Aws.Util.option_map (Aws.Json.lookup j "LastDecreaseDateTime")
             DateTime.of_json);
        number_of_decreases_today =
          (Aws.Util.option_map (Aws.Json.lookup j "NumberOfDecreasesToday")
             Long.of_json);
        read_capacity_units =
          (Aws.Util.option_map (Aws.Json.lookup j "ReadCapacityUnits")
             Long.of_json);
        write_capacity_units =
          (Aws.Util.option_map (Aws.Json.lookup j "WriteCapacityUnits")
             Long.of_json)
      }
  end
module LocalSecondaryIndexDescription =
  struct
    type t =
      {
      index_name: String.t option ;
      key_schema: KeySchema.t ;
      projection: Projection.t option ;
      index_size_bytes: Long.t option ;
      item_count: Long.t option ;
      index_arn: String.t option }
    let make ?index_name  ?(key_schema= [])  ?projection  ?index_size_bytes 
      ?item_count  ?index_arn  () =
      {
        index_name;
        key_schema;
        projection;
        index_size_bytes;
        item_count;
        index_arn
      }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Util.option_bind (Aws.Xml.member "IndexName" xml)
               String.parse);
          key_schema =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "KeySchema" xml)
                  KeySchema.parse));
          projection =
            (Aws.Util.option_bind (Aws.Xml.member "Projection" xml)
               Projection.parse);
          index_size_bytes =
            (Aws.Util.option_bind (Aws.Xml.member "IndexSizeBytes" xml)
               Long.parse);
          item_count =
            (Aws.Util.option_bind (Aws.Xml.member "ItemCount" xml) Long.parse);
          index_arn =
            (Aws.Util.option_bind (Aws.Xml.member "IndexArn" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.index_arn
              (fun f -> Aws.Query.Pair ("IndexArn", (String.to_query f)));
           Aws.Util.option_map v.item_count
             (fun f -> Aws.Query.Pair ("ItemCount", (Long.to_query f)));
           Aws.Util.option_map v.index_size_bytes
             (fun f -> Aws.Query.Pair ("IndexSizeBytes", (Long.to_query f)));
           Aws.Util.option_map v.projection
             (fun f -> Aws.Query.Pair ("Projection", (Projection.to_query f)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Aws.Util.option_map v.index_name
             (fun f -> Aws.Query.Pair ("IndexName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.index_arn
              (fun f -> ("IndexArn", (String.to_json f)));
           Aws.Util.option_map v.item_count
             (fun f -> ("ItemCount", (Long.to_json f)));
           Aws.Util.option_map v.index_size_bytes
             (fun f -> ("IndexSizeBytes", (Long.to_json f)));
           Aws.Util.option_map v.projection
             (fun f -> ("Projection", (Projection.to_json f)));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Aws.Util.option_map v.index_name
             (fun f -> ("IndexName", (String.to_json f)))])
    let of_json j =
      {
        index_name =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexName") String.of_json);
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        projection =
          (Aws.Util.option_map (Aws.Json.lookup j "Projection")
             Projection.of_json);
        index_size_bytes =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexSizeBytes")
             Long.of_json);
        item_count =
          (Aws.Util.option_map (Aws.Json.lookup j "ItemCount") Long.of_json);
        index_arn =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexArn") String.of_json)
      }
  end
module LocalSecondaryIndexDescriptionList =
  struct
    type t = LocalSecondaryIndexDescription.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map LocalSecondaryIndexDescription.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list LocalSecondaryIndexDescription.to_query v
    let to_json v = `List (List.map LocalSecondaryIndexDescription.to_json v)
    let of_json j = Aws.Json.to_list LocalSecondaryIndexDescription.of_json j
  end
module IndexStatus =
  struct
    type t =
      | CREATING 
      | UPDATING 
      | DELETING 
      | ACTIVE 
    let str_to_t =
      [("ACTIVE", ACTIVE);
      ("DELETING", DELETING);
      ("UPDATING", UPDATING);
      ("CREATING", CREATING)]
    let t_to_str =
      [(ACTIVE, "ACTIVE");
      (DELETING, "DELETING");
      (UPDATING, "UPDATING");
      (CREATING, "CREATING")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module GlobalSecondaryIndexDescription =
  struct
    type t =
      {
      index_name: String.t option ;
      key_schema: KeySchema.t ;
      projection: Projection.t option ;
      index_status: IndexStatus.t option ;
      backfilling: Boolean.t option ;
      provisioned_throughput: ProvisionedThroughputDescription.t option ;
      index_size_bytes: Long.t option ;
      item_count: Long.t option ;
      index_arn: String.t option }
    let make ?index_name  ?(key_schema= [])  ?projection  ?index_status 
      ?backfilling  ?provisioned_throughput  ?index_size_bytes  ?item_count 
      ?index_arn  () =
      {
        index_name;
        key_schema;
        projection;
        index_status;
        backfilling;
        provisioned_throughput;
        index_size_bytes;
        item_count;
        index_arn
      }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Util.option_bind (Aws.Xml.member "IndexName" xml)
               String.parse);
          key_schema =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "KeySchema" xml)
                  KeySchema.parse));
          projection =
            (Aws.Util.option_bind (Aws.Xml.member "Projection" xml)
               Projection.parse);
          index_status =
            (Aws.Util.option_bind (Aws.Xml.member "IndexStatus" xml)
               IndexStatus.parse);
          backfilling =
            (Aws.Util.option_bind (Aws.Xml.member "Backfilling" xml)
               Boolean.parse);
          provisioned_throughput =
            (Aws.Util.option_bind
               (Aws.Xml.member "ProvisionedThroughput" xml)
               ProvisionedThroughputDescription.parse);
          index_size_bytes =
            (Aws.Util.option_bind (Aws.Xml.member "IndexSizeBytes" xml)
               Long.parse);
          item_count =
            (Aws.Util.option_bind (Aws.Xml.member "ItemCount" xml) Long.parse);
          index_arn =
            (Aws.Util.option_bind (Aws.Xml.member "IndexArn" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.index_arn
              (fun f -> Aws.Query.Pair ("IndexArn", (String.to_query f)));
           Aws.Util.option_map v.item_count
             (fun f -> Aws.Query.Pair ("ItemCount", (Long.to_query f)));
           Aws.Util.option_map v.index_size_bytes
             (fun f -> Aws.Query.Pair ("IndexSizeBytes", (Long.to_query f)));
           Aws.Util.option_map v.provisioned_throughput
             (fun f ->
                Aws.Query.Pair
                  ("ProvisionedThroughput",
                    (ProvisionedThroughputDescription.to_query f)));
           Aws.Util.option_map v.backfilling
             (fun f -> Aws.Query.Pair ("Backfilling", (Boolean.to_query f)));
           Aws.Util.option_map v.index_status
             (fun f ->
                Aws.Query.Pair ("IndexStatus", (IndexStatus.to_query f)));
           Aws.Util.option_map v.projection
             (fun f -> Aws.Query.Pair ("Projection", (Projection.to_query f)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Aws.Util.option_map v.index_name
             (fun f -> Aws.Query.Pair ("IndexName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.index_arn
              (fun f -> ("IndexArn", (String.to_json f)));
           Aws.Util.option_map v.item_count
             (fun f -> ("ItemCount", (Long.to_json f)));
           Aws.Util.option_map v.index_size_bytes
             (fun f -> ("IndexSizeBytes", (Long.to_json f)));
           Aws.Util.option_map v.provisioned_throughput
             (fun f ->
                ("ProvisionedThroughput",
                  (ProvisionedThroughputDescription.to_json f)));
           Aws.Util.option_map v.backfilling
             (fun f -> ("Backfilling", (Boolean.to_json f)));
           Aws.Util.option_map v.index_status
             (fun f -> ("IndexStatus", (IndexStatus.to_json f)));
           Aws.Util.option_map v.projection
             (fun f -> ("Projection", (Projection.to_json f)));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Aws.Util.option_map v.index_name
             (fun f -> ("IndexName", (String.to_json f)))])
    let of_json j =
      {
        index_name =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexName") String.of_json);
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        projection =
          (Aws.Util.option_map (Aws.Json.lookup j "Projection")
             Projection.of_json);
        index_status =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexStatus")
             IndexStatus.of_json);
        backfilling =
          (Aws.Util.option_map (Aws.Json.lookup j "Backfilling")
             Boolean.of_json);
        provisioned_throughput =
          (Aws.Util.option_map (Aws.Json.lookup j "ProvisionedThroughput")
             ProvisionedThroughputDescription.of_json);
        index_size_bytes =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexSizeBytes")
             Long.of_json);
        item_count =
          (Aws.Util.option_map (Aws.Json.lookup j "ItemCount") Long.of_json);
        index_arn =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexArn") String.of_json)
      }
  end
module GlobalSecondaryIndexDescriptionList =
  struct
    type t = GlobalSecondaryIndexDescription.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map GlobalSecondaryIndexDescription.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list GlobalSecondaryIndexDescription.to_query v
    let to_json v =
      `List (List.map GlobalSecondaryIndexDescription.to_json v)
    let of_json j =
      Aws.Json.to_list GlobalSecondaryIndexDescription.of_json j
  end
module TableDescription =
  struct
    type t =
      {
      attribute_definitions: AttributeDefinitions.t ;
      table_name: String.t option ;
      key_schema: KeySchema.t ;
      table_status: TableStatus.t option ;
      creation_date_time: DateTime.t option ;
      provisioned_throughput: ProvisionedThroughputDescription.t option ;
      table_size_bytes: Long.t option ;
      item_count: Long.t option ;
      table_arn: String.t option ;
      local_secondary_indexes: LocalSecondaryIndexDescriptionList.t ;
      global_secondary_indexes: GlobalSecondaryIndexDescriptionList.t ;
      stream_specification: StreamSpecification.t option ;
      latest_stream_label: String.t option ;
      latest_stream_arn: String.t option }
    let make ?(attribute_definitions= [])  ?table_name  ?(key_schema= []) 
      ?table_status  ?creation_date_time  ?provisioned_throughput 
      ?table_size_bytes  ?item_count  ?table_arn  ?(local_secondary_indexes=
      [])  ?(global_secondary_indexes= [])  ?stream_specification 
      ?latest_stream_label  ?latest_stream_arn  () =
      {
        attribute_definitions;
        table_name;
        key_schema;
        table_status;
        creation_date_time;
        provisioned_throughput;
        table_size_bytes;
        item_count;
        table_arn;
        local_secondary_indexes;
        global_secondary_indexes;
        stream_specification;
        latest_stream_label;
        latest_stream_arn
      }
    let parse xml =
      Some
        {
          attribute_definitions =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "AttributeDefinitions" xml)
                  AttributeDefinitions.parse));
          table_name =
            (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
               String.parse);
          key_schema =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "KeySchema" xml)
                  KeySchema.parse));
          table_status =
            (Aws.Util.option_bind (Aws.Xml.member "TableStatus" xml)
               TableStatus.parse);
          creation_date_time =
            (Aws.Util.option_bind (Aws.Xml.member "CreationDateTime" xml)
               DateTime.parse);
          provisioned_throughput =
            (Aws.Util.option_bind
               (Aws.Xml.member "ProvisionedThroughput" xml)
               ProvisionedThroughputDescription.parse);
          table_size_bytes =
            (Aws.Util.option_bind (Aws.Xml.member "TableSizeBytes" xml)
               Long.parse);
          item_count =
            (Aws.Util.option_bind (Aws.Xml.member "ItemCount" xml) Long.parse);
          table_arn =
            (Aws.Util.option_bind (Aws.Xml.member "TableArn" xml)
               String.parse);
          local_secondary_indexes =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "LocalSecondaryIndexes" xml)
                  LocalSecondaryIndexDescriptionList.parse));
          global_secondary_indexes =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "GlobalSecondaryIndexes" xml)
                  GlobalSecondaryIndexDescriptionList.parse));
          stream_specification =
            (Aws.Util.option_bind (Aws.Xml.member "StreamSpecification" xml)
               StreamSpecification.parse);
          latest_stream_label =
            (Aws.Util.option_bind (Aws.Xml.member "LatestStreamLabel" xml)
               String.parse);
          latest_stream_arn =
            (Aws.Util.option_bind (Aws.Xml.member "LatestStreamArn" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.latest_stream_arn
              (fun f ->
                 Aws.Query.Pair ("LatestStreamArn", (String.to_query f)));
           Aws.Util.option_map v.latest_stream_label
             (fun f ->
                Aws.Query.Pair ("LatestStreamLabel", (String.to_query f)));
           Aws.Util.option_map v.stream_specification
             (fun f ->
                Aws.Query.Pair
                  ("StreamSpecification", (StreamSpecification.to_query f)));
           Some
             (Aws.Query.Pair
                ("GlobalSecondaryIndexes.member",
                  (GlobalSecondaryIndexDescriptionList.to_query
                     v.global_secondary_indexes)));
           Some
             (Aws.Query.Pair
                ("LocalSecondaryIndexes.member",
                  (LocalSecondaryIndexDescriptionList.to_query
                     v.local_secondary_indexes)));
           Aws.Util.option_map v.table_arn
             (fun f -> Aws.Query.Pair ("TableArn", (String.to_query f)));
           Aws.Util.option_map v.item_count
             (fun f -> Aws.Query.Pair ("ItemCount", (Long.to_query f)));
           Aws.Util.option_map v.table_size_bytes
             (fun f -> Aws.Query.Pair ("TableSizeBytes", (Long.to_query f)));
           Aws.Util.option_map v.provisioned_throughput
             (fun f ->
                Aws.Query.Pair
                  ("ProvisionedThroughput",
                    (ProvisionedThroughputDescription.to_query f)));
           Aws.Util.option_map v.creation_date_time
             (fun f ->
                Aws.Query.Pair ("CreationDateTime", (DateTime.to_query f)));
           Aws.Util.option_map v.table_status
             (fun f ->
                Aws.Query.Pair ("TableStatus", (TableStatus.to_query f)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Aws.Util.option_map v.table_name
             (fun f -> Aws.Query.Pair ("TableName", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("AttributeDefinitions.member",
                  (AttributeDefinitions.to_query v.attribute_definitions)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.latest_stream_arn
              (fun f -> ("LatestStreamArn", (String.to_json f)));
           Aws.Util.option_map v.latest_stream_label
             (fun f -> ("LatestStreamLabel", (String.to_json f)));
           Aws.Util.option_map v.stream_specification
             (fun f ->
                ("StreamSpecification", (StreamSpecification.to_json f)));
           Some
             ("GlobalSecondaryIndexes",
               (GlobalSecondaryIndexDescriptionList.to_json
                  v.global_secondary_indexes));
           Some
             ("LocalSecondaryIndexes",
               (LocalSecondaryIndexDescriptionList.to_json
                  v.local_secondary_indexes));
           Aws.Util.option_map v.table_arn
             (fun f -> ("TableArn", (String.to_json f)));
           Aws.Util.option_map v.item_count
             (fun f -> ("ItemCount", (Long.to_json f)));
           Aws.Util.option_map v.table_size_bytes
             (fun f -> ("TableSizeBytes", (Long.to_json f)));
           Aws.Util.option_map v.provisioned_throughput
             (fun f ->
                ("ProvisionedThroughput",
                  (ProvisionedThroughputDescription.to_json f)));
           Aws.Util.option_map v.creation_date_time
             (fun f -> ("CreationDateTime", (DateTime.to_json f)));
           Aws.Util.option_map v.table_status
             (fun f -> ("TableStatus", (TableStatus.to_json f)));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Aws.Util.option_map v.table_name
             (fun f -> ("TableName", (String.to_json f)));
           Some
             ("AttributeDefinitions",
               (AttributeDefinitions.to_json v.attribute_definitions))])
    let of_json j =
      {
        attribute_definitions =
          (AttributeDefinitions.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "AttributeDefinitions")));
        table_name =
          (Aws.Util.option_map (Aws.Json.lookup j "TableName") String.of_json);
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        table_status =
          (Aws.Util.option_map (Aws.Json.lookup j "TableStatus")
             TableStatus.of_json);
        creation_date_time =
          (Aws.Util.option_map (Aws.Json.lookup j "CreationDateTime")
             DateTime.of_json);
        provisioned_throughput =
          (Aws.Util.option_map (Aws.Json.lookup j "ProvisionedThroughput")
             ProvisionedThroughputDescription.of_json);
        table_size_bytes =
          (Aws.Util.option_map (Aws.Json.lookup j "TableSizeBytes")
             Long.of_json);
        item_count =
          (Aws.Util.option_map (Aws.Json.lookup j "ItemCount") Long.of_json);
        table_arn =
          (Aws.Util.option_map (Aws.Json.lookup j "TableArn") String.of_json);
        local_secondary_indexes =
          (LocalSecondaryIndexDescriptionList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "LocalSecondaryIndexes")));
        global_secondary_indexes =
          (GlobalSecondaryIndexDescriptionList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "GlobalSecondaryIndexes")));
        stream_specification =
          (Aws.Util.option_map (Aws.Json.lookup j "StreamSpecification")
             StreamSpecification.of_json);
        latest_stream_label =
          (Aws.Util.option_map (Aws.Json.lookup j "LatestStreamLabel")
             String.of_json);
        latest_stream_arn =
          (Aws.Util.option_map (Aws.Json.lookup j "LatestStreamArn")
             String.of_json)
      }
  end
module UpdateTableOutput =
  struct
    type t = {
      table_description: TableDescription.t option }
    let make ?table_description  () = { table_description }
    let parse xml =
      Some
        {
          table_description =
            (Aws.Util.option_bind (Aws.Xml.member "TableDescription" xml)
               TableDescription.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.table_description
              (fun f ->
                 Aws.Query.Pair
                   ("TableDescription", (TableDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.table_description
              (fun f -> ("TableDescription", (TableDescription.to_json f)))])
    let of_json j =
      {
        table_description =
          (Aws.Util.option_map (Aws.Json.lookup j "TableDescription")
             TableDescription.of_json)
      }
  end
module AttributeMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ItemList =
  struct
    type t = AttributeMap.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map AttributeMap.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list AttributeMap.to_query v
    let to_json v = `List (List.map AttributeMap.to_json v)
    let of_json j = Aws.Json.to_list AttributeMap.of_json j
  end
module BatchGetResponseMap =
  struct
    type t = (String.t, ItemList.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string ItemList.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (ItemList.to_json v)) ::
                  acc) v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string ItemList.of_json j
  end
module ProvisionedThroughputExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      {
        message =
          (Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json)
      }
  end
module ConditionalOperator =
  struct
    type t =
      | AND 
      | OR 
    let str_to_t = [("OR", OR); ("AND", AND)]
    let t_to_str = [(OR, "OR"); (AND, "AND")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AttributeValueList =
  struct
    type t = AttributeValue.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map AttributeValue.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list AttributeValue.to_query v
    let to_json v = `List (List.map AttributeValue.to_json v)
    let of_json j = Aws.Json.to_list AttributeValue.of_json j
  end
module Select =
  struct
    type t =
      | ALL_ATTRIBUTES 
      | ALL_PROJECTED_ATTRIBUTES 
      | SPECIFIC_ATTRIBUTES 
      | COUNT 
    let str_to_t =
      [("COUNT", COUNT);
      ("SPECIFIC_ATTRIBUTES", SPECIFIC_ATTRIBUTES);
      ("ALL_PROJECTED_ATTRIBUTES", ALL_PROJECTED_ATTRIBUTES);
      ("ALL_ATTRIBUTES", ALL_ATTRIBUTES)]
    let t_to_str =
      [(COUNT, "COUNT");
      (SPECIFIC_ATTRIBUTES, "SPECIFIC_ATTRIBUTES");
      (ALL_PROJECTED_ATTRIBUTES, "ALL_PROJECTED_ATTRIBUTES");
      (ALL_ATTRIBUTES, "ALL_ATTRIBUTES")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module DescribeTableOutput =
  struct
    type t = {
      table: TableDescription.t option }
    let make ?table  () = { table }
    let parse xml =
      Some
        {
          table =
            (Aws.Util.option_bind (Aws.Xml.member "Table" xml)
               TableDescription.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.table
              (fun f ->
                 Aws.Query.Pair ("Table", (TableDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.table
              (fun f -> ("Table", (TableDescription.to_json f)))])
    let of_json j =
      {
        table =
          (Aws.Util.option_map (Aws.Json.lookup j "Table")
             TableDescription.of_json)
      }
  end
module ItemCollectionSizeLimitExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      {
        message =
          (Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json)
      }
  end
module Capacity =
  struct
    type t = {
      capacity_units: Double.t option }
    let make ?capacity_units  () = { capacity_units }
    let parse xml =
      Some
        {
          capacity_units =
            (Aws.Util.option_bind (Aws.Xml.member "CapacityUnits" xml)
               Double.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.capacity_units
              (fun f -> Aws.Query.Pair ("CapacityUnits", (Double.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.capacity_units
              (fun f -> ("CapacityUnits", (Double.to_json f)))])
    let of_json j =
      {
        capacity_units =
          (Aws.Util.option_map (Aws.Json.lookup j "CapacityUnits")
             Double.of_json)
      }
  end
module SecondaryIndexesCapacityMap =
  struct
    type t = (String.t, Capacity.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string Capacity.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Capacity.to_json v)) ::
                  acc) v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string Capacity.of_json j
  end
module ConsumedCapacity =
  struct
    type t =
      {
      table_name: String.t option ;
      capacity_units: Double.t option ;
      table: Capacity.t option ;
      local_secondary_indexes: SecondaryIndexesCapacityMap.t option ;
      global_secondary_indexes: SecondaryIndexesCapacityMap.t option }
    let make ?table_name  ?capacity_units  ?table  ?local_secondary_indexes 
      ?global_secondary_indexes  () =
      {
        table_name;
        capacity_units;
        table;
        local_secondary_indexes;
        global_secondary_indexes
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
               String.parse);
          capacity_units =
            (Aws.Util.option_bind (Aws.Xml.member "CapacityUnits" xml)
               Double.parse);
          table =
            (Aws.Util.option_bind (Aws.Xml.member "Table" xml) Capacity.parse);
          local_secondary_indexes =
            (Aws.Util.option_bind
               (Aws.Xml.member "LocalSecondaryIndexes" xml)
               SecondaryIndexesCapacityMap.parse);
          global_secondary_indexes =
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalSecondaryIndexes" xml)
               SecondaryIndexesCapacityMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.global_secondary_indexes
              (fun f ->
                 Aws.Query.Pair
                   ("GlobalSecondaryIndexes",
                     (SecondaryIndexesCapacityMap.to_query f)));
           Aws.Util.option_map v.local_secondary_indexes
             (fun f ->
                Aws.Query.Pair
                  ("LocalSecondaryIndexes",
                    (SecondaryIndexesCapacityMap.to_query f)));
           Aws.Util.option_map v.table
             (fun f -> Aws.Query.Pair ("Table", (Capacity.to_query f)));
           Aws.Util.option_map v.capacity_units
             (fun f -> Aws.Query.Pair ("CapacityUnits", (Double.to_query f)));
           Aws.Util.option_map v.table_name
             (fun f -> Aws.Query.Pair ("TableName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.global_secondary_indexes
              (fun f ->
                 ("GlobalSecondaryIndexes",
                   (SecondaryIndexesCapacityMap.to_json f)));
           Aws.Util.option_map v.local_secondary_indexes
             (fun f ->
                ("LocalSecondaryIndexes",
                  (SecondaryIndexesCapacityMap.to_json f)));
           Aws.Util.option_map v.table
             (fun f -> ("Table", (Capacity.to_json f)));
           Aws.Util.option_map v.capacity_units
             (fun f -> ("CapacityUnits", (Double.to_json f)));
           Aws.Util.option_map v.table_name
             (fun f -> ("TableName", (String.to_json f)))])
    let of_json j =
      {
        table_name =
          (Aws.Util.option_map (Aws.Json.lookup j "TableName") String.of_json);
        capacity_units =
          (Aws.Util.option_map (Aws.Json.lookup j "CapacityUnits")
             Double.of_json);
        table =
          (Aws.Util.option_map (Aws.Json.lookup j "Table") Capacity.of_json);
        local_secondary_indexes =
          (Aws.Util.option_map (Aws.Json.lookup j "LocalSecondaryIndexes")
             SecondaryIndexesCapacityMap.of_json);
        global_secondary_indexes =
          (Aws.Util.option_map (Aws.Json.lookup j "GlobalSecondaryIndexes")
             SecondaryIndexesCapacityMap.of_json)
      }
  end
module ConsumedCapacityMultiple =
  struct
    type t = ConsumedCapacity.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map ConsumedCapacity.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ConsumedCapacity.to_query v
    let to_json v = `List (List.map ConsumedCapacity.to_json v)
    let of_json j = Aws.Json.to_list ConsumedCapacity.of_json j
  end
module Key =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module KeyList =
  struct
    type t = Key.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all (List.map Key.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Key.to_query v
    let to_json v = `List (List.map Key.to_json v)
    let of_json j = Aws.Json.to_list Key.of_json j
  end
module ExpressionAttributeNameMap =
  struct
    type t = (String.t, String.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string String.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (String.to_json v)) :: acc)
           v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string String.of_json j
  end
module AttributeNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module KeysAndAttributes =
  struct
    type t =
      {
      keys: KeyList.t ;
      attributes_to_get: AttributeNameList.t ;
      consistent_read: Boolean.t option ;
      projection_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option }
    let make ~keys  ?(attributes_to_get= [])  ?consistent_read 
      ?projection_expression  ?expression_attribute_names  () =
      {
        keys;
        attributes_to_get;
        consistent_read;
        projection_expression;
        expression_attribute_names
      }
    let parse xml =
      Some
        {
          keys =
            (Aws.Xml.required "Keys"
               (Aws.Util.option_bind (Aws.Xml.member "Keys" xml)
                  KeyList.parse));
          attributes_to_get =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "AttributesToGet" xml)
                  AttributeNameList.parse));
          consistent_read =
            (Aws.Util.option_bind (Aws.Xml.member "ConsistentRead" xml)
               Boolean.parse);
          projection_expression =
            (Aws.Util.option_bind (Aws.Xml.member "ProjectionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_names
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeNames",
                     (ExpressionAttributeNameMap.to_query f)));
           Aws.Util.option_map v.projection_expression
             (fun f ->
                Aws.Query.Pair ("ProjectionExpression", (String.to_query f)));
           Aws.Util.option_map v.consistent_read
             (fun f ->
                Aws.Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Some
             (Aws.Query.Pair
                ("AttributesToGet.member",
                  (AttributeNameList.to_query v.attributes_to_get)));
           Some (Aws.Query.Pair ("Keys.member", (KeyList.to_query v.keys)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_names
              (fun f ->
                 ("ExpressionAttributeNames",
                   (ExpressionAttributeNameMap.to_json f)));
           Aws.Util.option_map v.projection_expression
             (fun f -> ("ProjectionExpression", (String.to_json f)));
           Aws.Util.option_map v.consistent_read
             (fun f -> ("ConsistentRead", (Boolean.to_json f)));
           Some
             ("AttributesToGet",
               (AttributeNameList.to_json v.attributes_to_get));
           Some ("Keys", (KeyList.to_json v.keys))])
    let of_json j =
      {
        keys =
          (KeyList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Keys")));
        attributes_to_get =
          (AttributeNameList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributesToGet")));
        consistent_read =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsistentRead")
             Boolean.of_json);
        projection_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "ProjectionExpression")
             String.of_json);
        expression_attribute_names =
          (Aws.Util.option_map (Aws.Json.lookup j "ExpressionAttributeNames")
             ExpressionAttributeNameMap.of_json)
      }
  end
module BatchGetRequestMap =
  struct
    type t = (String.t, KeysAndAttributes.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string KeysAndAttributes.to_query
        v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (KeysAndAttributes.to_json v)) ::
                  acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string KeysAndAttributes.of_json j
  end
module BatchGetItemOutput =
  struct
    type t =
      {
      responses: BatchGetResponseMap.t option ;
      unprocessed_keys: BatchGetRequestMap.t option ;
      consumed_capacity: ConsumedCapacityMultiple.t }
    let make ?responses  ?unprocessed_keys  ?(consumed_capacity= [])  () =
      { responses; unprocessed_keys; consumed_capacity }
    let parse xml =
      Some
        {
          responses =
            (Aws.Util.option_bind (Aws.Xml.member "Responses" xml)
               BatchGetResponseMap.parse);
          unprocessed_keys =
            (Aws.Util.option_bind (Aws.Xml.member "UnprocessedKeys" xml)
               BatchGetRequestMap.parse);
          consumed_capacity =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "ConsumedCapacity" xml)
                  ConsumedCapacityMultiple.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("ConsumedCapacity.member",
                   (ConsumedCapacityMultiple.to_query v.consumed_capacity)));
           Aws.Util.option_map v.unprocessed_keys
             (fun f ->
                Aws.Query.Pair
                  ("UnprocessedKeys", (BatchGetRequestMap.to_query f)));
           Aws.Util.option_map v.responses
             (fun f ->
                Aws.Query.Pair
                  ("Responses", (BatchGetResponseMap.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("ConsumedCapacity",
                (ConsumedCapacityMultiple.to_json v.consumed_capacity));
           Aws.Util.option_map v.unprocessed_keys
             (fun f -> ("UnprocessedKeys", (BatchGetRequestMap.to_json f)));
           Aws.Util.option_map v.responses
             (fun f -> ("Responses", (BatchGetResponseMap.to_json f)))])
    let of_json j =
      {
        responses =
          (Aws.Util.option_map (Aws.Json.lookup j "Responses")
             BatchGetResponseMap.of_json);
        unprocessed_keys =
          (Aws.Util.option_map (Aws.Json.lookup j "UnprocessedKeys")
             BatchGetRequestMap.of_json);
        consumed_capacity =
          (ConsumedCapacityMultiple.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ConsumedCapacity")))
      }
  end
module ComparisonOperator =
  struct
    type t =
      | EQ 
      | NE 
      | IN 
      | LE 
      | LT 
      | GE 
      | GT 
      | BETWEEN 
      | NOT_NULL 
      | NULL 
      | CONTAINS 
      | NOT_CONTAINS 
      | BEGINS_WITH 
    let str_to_t =
      [("BEGINS_WITH", BEGINS_WITH);
      ("NOT_CONTAINS", NOT_CONTAINS);
      ("CONTAINS", CONTAINS);
      ("NULL", NULL);
      ("NOT_NULL", NOT_NULL);
      ("BETWEEN", BETWEEN);
      ("GT", GT);
      ("GE", GE);
      ("LT", LT);
      ("LE", LE);
      ("IN", IN);
      ("NE", NE);
      ("EQ", EQ)]
    let t_to_str =
      [(BEGINS_WITH, "BEGINS_WITH");
      (NOT_CONTAINS, "NOT_CONTAINS");
      (CONTAINS, "CONTAINS");
      (NULL, "NULL");
      (NOT_NULL, "NOT_NULL");
      (BETWEEN, "BETWEEN");
      (GT, "GT");
      (GE, "GE");
      (LT, "LT");
      (LE, "LE");
      (IN, "IN");
      (NE, "NE");
      (EQ, "EQ")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Condition =
  struct
    type t =
      {
      attribute_value_list: AttributeValueList.t ;
      comparison_operator: ComparisonOperator.t }
    let make ?(attribute_value_list= [])  ~comparison_operator  () =
      { attribute_value_list; comparison_operator }
    let parse xml =
      Some
        {
          attribute_value_list =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "AttributeValueList" xml)
                  AttributeValueList.parse));
          comparison_operator =
            (Aws.Xml.required "ComparisonOperator"
               (Aws.Util.option_bind
                  (Aws.Xml.member "ComparisonOperator" xml)
                  ComparisonOperator.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("ComparisonOperator",
                   (ComparisonOperator.to_query v.comparison_operator)));
           Some
             (Aws.Query.Pair
                ("AttributeValueList.member",
                  (AttributeValueList.to_query v.attribute_value_list)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("ComparisonOperator",
                (ComparisonOperator.to_json v.comparison_operator));
           Some
             ("AttributeValueList",
               (AttributeValueList.to_json v.attribute_value_list))])
    let of_json j =
      {
        attribute_value_list =
          (AttributeValueList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeValueList")));
        comparison_operator =
          (ComparisonOperator.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ComparisonOperator")))
      }
  end
module KeyConditions =
  struct
    type t = (String.t, Condition.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string Condition.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Condition.to_json v)) ::
                  acc) v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string Condition.of_json j
  end
module ReturnValue =
  struct
    type t =
      | NONE 
      | ALL_OLD 
      | UPDATED_OLD 
      | ALL_NEW 
      | UPDATED_NEW 
    let str_to_t =
      [("UPDATED_NEW", UPDATED_NEW);
      ("ALL_NEW", ALL_NEW);
      ("UPDATED_OLD", UPDATED_OLD);
      ("ALL_OLD", ALL_OLD);
      ("NONE", NONE)]
    let t_to_str =
      [(UPDATED_NEW, "UPDATED_NEW");
      (ALL_NEW, "ALL_NEW");
      (UPDATED_OLD, "UPDATED_OLD");
      (ALL_OLD, "ALL_OLD");
      (NONE, "NONE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ReturnItemCollectionMetrics =
  struct
    type t =
      | SIZE 
      | NONE 
    let str_to_t = [("NONE", NONE); ("SIZE", SIZE)]
    let t_to_str = [(NONE, "NONE"); (SIZE, "SIZE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ReturnConsumedCapacity =
  struct
    type t =
      | INDEXES 
      | TOTAL 
      | NONE 
    let str_to_t = [("NONE", NONE); ("TOTAL", TOTAL); ("INDEXES", INDEXES)]
    let t_to_str = [(NONE, "NONE"); (TOTAL, "TOTAL"); (INDEXES, "INDEXES")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module PutItemInputAttributeMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ExpressionAttributeValueMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ExpectedAttributeValue =
  struct
    type t =
      {
      value: AttributeValue.t option ;
      exists: Boolean.t option ;
      comparison_operator: ComparisonOperator.t option ;
      attribute_value_list: AttributeValueList.t }
    let make ?value  ?exists  ?comparison_operator  ?(attribute_value_list=
      [])  () = { value; exists; comparison_operator; attribute_value_list }
    let parse xml =
      Some
        {
          value =
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml)
               AttributeValue.parse);
          exists =
            (Aws.Util.option_bind (Aws.Xml.member "Exists" xml) Boolean.parse);
          comparison_operator =
            (Aws.Util.option_bind (Aws.Xml.member "ComparisonOperator" xml)
               ComparisonOperator.parse);
          attribute_value_list =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "AttributeValueList" xml)
                  AttributeValueList.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("AttributeValueList.member",
                   (AttributeValueList.to_query v.attribute_value_list)));
           Aws.Util.option_map v.comparison_operator
             (fun f ->
                Aws.Query.Pair
                  ("ComparisonOperator", (ComparisonOperator.to_query f)));
           Aws.Util.option_map v.exists
             (fun f -> Aws.Query.Pair ("Exists", (Boolean.to_query f)));
           Aws.Util.option_map v.value
             (fun f -> Aws.Query.Pair ("Value", (AttributeValue.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("AttributeValueList",
                (AttributeValueList.to_json v.attribute_value_list));
           Aws.Util.option_map v.comparison_operator
             (fun f -> ("ComparisonOperator", (ComparisonOperator.to_json f)));
           Aws.Util.option_map v.exists
             (fun f -> ("Exists", (Boolean.to_json f)));
           Aws.Util.option_map v.value
             (fun f -> ("Value", (AttributeValue.to_json f)))])
    let of_json j =
      {
        value =
          (Aws.Util.option_map (Aws.Json.lookup j "Value")
             AttributeValue.of_json);
        exists =
          (Aws.Util.option_map (Aws.Json.lookup j "Exists") Boolean.of_json);
        comparison_operator =
          (Aws.Util.option_map (Aws.Json.lookup j "ComparisonOperator")
             ComparisonOperator.of_json);
        attribute_value_list =
          (AttributeValueList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeValueList")))
      }
  end
module ExpectedAttributeMap =
  struct
    type t = (String.t, ExpectedAttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string
        ExpectedAttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (ExpectedAttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string ExpectedAttributeValue.of_json j
  end
module PutItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      item: PutItemInputAttributeMap.t ;
      expected: ExpectedAttributeMap.t option ;
      return_values: ReturnValue.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option ;
      conditional_operator: ConditionalOperator.t option ;
      condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ~item  ?expected  ?return_values 
      ?return_consumed_capacity  ?return_item_collection_metrics 
      ?conditional_operator  ?condition_expression 
      ?expression_attribute_names  ?expression_attribute_values  () =
      {
        table_name;
        item;
        expected;
        return_values;
        return_consumed_capacity;
        return_item_collection_metrics;
        conditional_operator;
        condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse));
          item =
            (Aws.Xml.required "Item"
               (Aws.Util.option_bind (Aws.Xml.member "Item" xml)
                  PutItemInputAttributeMap.parse));
          expected =
            (Aws.Util.option_bind (Aws.Xml.member "Expected" xml)
               ExpectedAttributeMap.parse);
          return_values =
            (Aws.Util.option_bind (Aws.Xml.member "ReturnValues" xml)
               ReturnValue.parse);
          return_consumed_capacity =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse);
          conditional_operator =
            (Aws.Util.option_bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          condition_expression =
            (Aws.Util.option_bind (Aws.Xml.member "ConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_values
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Aws.Util.option_map v.condition_expression
             (fun f ->
                Aws.Query.Pair ("ConditionExpression", (String.to_query f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Aws.Util.option_map v.return_item_collection_metrics
             (fun f ->
                Aws.Query.Pair
                  ("ReturnItemCollectionMetrics",
                    (ReturnItemCollectionMetrics.to_query f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Aws.Util.option_map v.return_values
             (fun f ->
                Aws.Query.Pair ("ReturnValues", (ReturnValue.to_query f)));
           Aws.Util.option_map v.expected
             (fun f ->
                Aws.Query.Pair
                  ("Expected", (ExpectedAttributeMap.to_query f)));
           Some
             (Aws.Query.Pair
                ("Item", (PutItemInputAttributeMap.to_query v.item)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_values
              (fun f ->
                 ("ExpressionAttributeValues",
                   (ExpressionAttributeValueMap.to_json f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)));
           Aws.Util.option_map v.condition_expression
             (fun f -> ("ConditionExpression", (String.to_json f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)));
           Aws.Util.option_map v.return_item_collection_metrics
             (fun f ->
                ("ReturnItemCollectionMetrics",
                  (ReturnItemCollectionMetrics.to_json f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)));
           Aws.Util.option_map v.return_values
             (fun f -> ("ReturnValues", (ReturnValue.to_json f)));
           Aws.Util.option_map v.expected
             (fun f -> ("Expected", (ExpectedAttributeMap.to_json f)));
           Some ("Item", (PutItemInputAttributeMap.to_json v.item));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        item =
          (PutItemInputAttributeMap.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Item")));
        expected =
          (Aws.Util.option_map (Aws.Json.lookup j "Expected")
             ExpectedAttributeMap.of_json);
        return_values =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnValues")
             ReturnValue.of_json);
        return_consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnConsumedCapacity")
             ReturnConsumedCapacity.of_json);
        return_item_collection_metrics =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ReturnItemCollectionMetrics")
             ReturnItemCollectionMetrics.of_json);
        conditional_operator =
          (Aws.Util.option_map (Aws.Json.lookup j "ConditionalOperator")
             ConditionalOperator.of_json);
        condition_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "ConditionExpression")
             String.of_json);
        expression_attribute_names =
          (Aws.Util.option_map (Aws.Json.lookup j "ExpressionAttributeNames")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ExpressionAttributeValues")
             ExpressionAttributeValueMap.of_json)
      }
  end
module ResourceNotFoundException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      {
        message =
          (Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json)
      }
  end
module PutRequest =
  struct
    type t = {
      item: PutItemInputAttributeMap.t }
    let make ~item  () = { item }
    let parse xml =
      Some
        {
          item =
            (Aws.Xml.required "Item"
               (Aws.Util.option_bind (Aws.Xml.member "Item" xml)
                  PutItemInputAttributeMap.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Item", (PutItemInputAttributeMap.to_query v.item)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Item", (PutItemInputAttributeMap.to_json v.item))])
    let of_json j =
      {
        item =
          (PutItemInputAttributeMap.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Item")))
      }
  end
module DeleteRequest =
  struct
    type t = {
      key: Key.t }
    let make ~key  () = { key }
    let parse xml =
      Some
        {
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) Key.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Key", (Key.to_query v.key)))])
    let to_json v =
      `Assoc (Aws.Util.list_filter_opt [Some ("Key", (Key.to_json v.key))])
    let of_json j =
      {
        key =
          (Key.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")))
      }
  end
module WriteRequest =
  struct
    type t =
      {
      put_request: PutRequest.t option ;
      delete_request: DeleteRequest.t option }
    let make ?put_request  ?delete_request  () =
      { put_request; delete_request }
    let parse xml =
      Some
        {
          put_request =
            (Aws.Util.option_bind (Aws.Xml.member "PutRequest" xml)
               PutRequest.parse);
          delete_request =
            (Aws.Util.option_bind (Aws.Xml.member "DeleteRequest" xml)
               DeleteRequest.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.delete_request
              (fun f ->
                 Aws.Query.Pair ("DeleteRequest", (DeleteRequest.to_query f)));
           Aws.Util.option_map v.put_request
             (fun f -> Aws.Query.Pair ("PutRequest", (PutRequest.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.delete_request
              (fun f -> ("DeleteRequest", (DeleteRequest.to_json f)));
           Aws.Util.option_map v.put_request
             (fun f -> ("PutRequest", (PutRequest.to_json f)))])
    let of_json j =
      {
        put_request =
          (Aws.Util.option_map (Aws.Json.lookup j "PutRequest")
             PutRequest.of_json);
        delete_request =
          (Aws.Util.option_map (Aws.Json.lookup j "DeleteRequest")
             DeleteRequest.of_json)
      }
  end
module WriteRequests =
  struct
    type t = WriteRequest.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map WriteRequest.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list WriteRequest.to_query v
    let to_json v = `List (List.map WriteRequest.to_json v)
    let of_json j = Aws.Json.to_list WriteRequest.of_json j
  end
module BatchWriteItemRequestMap =
  struct
    type t = (String.t, WriteRequests.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string WriteRequests.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (WriteRequests.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string WriteRequests.of_json j
  end
module LimitExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      {
        message =
          (Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json)
      }
  end
module DeleteItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      key: Key.t ;
      expected: ExpectedAttributeMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      return_values: ReturnValue.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option ;
      condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ~key  ?expected  ?conditional_operator 
      ?return_values  ?return_consumed_capacity 
      ?return_item_collection_metrics  ?condition_expression 
      ?expression_attribute_names  ?expression_attribute_values  () =
      {
        table_name;
        key;
        expected;
        conditional_operator;
        return_values;
        return_consumed_capacity;
        return_item_collection_metrics;
        condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) Key.parse));
          expected =
            (Aws.Util.option_bind (Aws.Xml.member "Expected" xml)
               ExpectedAttributeMap.parse);
          conditional_operator =
            (Aws.Util.option_bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          return_values =
            (Aws.Util.option_bind (Aws.Xml.member "ReturnValues" xml)
               ReturnValue.parse);
          return_consumed_capacity =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse);
          condition_expression =
            (Aws.Util.option_bind (Aws.Xml.member "ConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_values
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Aws.Util.option_map v.condition_expression
             (fun f ->
                Aws.Query.Pair ("ConditionExpression", (String.to_query f)));
           Aws.Util.option_map v.return_item_collection_metrics
             (fun f ->
                Aws.Query.Pair
                  ("ReturnItemCollectionMetrics",
                    (ReturnItemCollectionMetrics.to_query f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Aws.Util.option_map v.return_values
             (fun f ->
                Aws.Query.Pair ("ReturnValues", (ReturnValue.to_query f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Aws.Util.option_map v.expected
             (fun f ->
                Aws.Query.Pair
                  ("Expected", (ExpectedAttributeMap.to_query f)));
           Some (Aws.Query.Pair ("Key", (Key.to_query v.key)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_values
              (fun f ->
                 ("ExpressionAttributeValues",
                   (ExpressionAttributeValueMap.to_json f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)));
           Aws.Util.option_map v.condition_expression
             (fun f -> ("ConditionExpression", (String.to_json f)));
           Aws.Util.option_map v.return_item_collection_metrics
             (fun f ->
                ("ReturnItemCollectionMetrics",
                  (ReturnItemCollectionMetrics.to_json f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)));
           Aws.Util.option_map v.return_values
             (fun f -> ("ReturnValues", (ReturnValue.to_json f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)));
           Aws.Util.option_map v.expected
             (fun f -> ("Expected", (ExpectedAttributeMap.to_json f)));
           Some ("Key", (Key.to_json v.key));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        key =
          (Key.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        expected =
          (Aws.Util.option_map (Aws.Json.lookup j "Expected")
             ExpectedAttributeMap.of_json);
        conditional_operator =
          (Aws.Util.option_map (Aws.Json.lookup j "ConditionalOperator")
             ConditionalOperator.of_json);
        return_values =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnValues")
             ReturnValue.of_json);
        return_consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnConsumedCapacity")
             ReturnConsumedCapacity.of_json);
        return_item_collection_metrics =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ReturnItemCollectionMetrics")
             ReturnItemCollectionMetrics.of_json);
        condition_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "ConditionExpression")
             String.of_json);
        expression_attribute_names =
          (Aws.Util.option_map (Aws.Json.lookup j "ExpressionAttributeNames")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ExpressionAttributeValues")
             ExpressionAttributeValueMap.of_json)
      }
  end
module FilterConditionMap =
  struct
    type t = (String.t, Condition.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string Condition.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Condition.to_json v)) ::
                  acc) v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string Condition.of_json j
  end
module ScanInput =
  struct
    type t =
      {
      table_name: String.t ;
      index_name: String.t option ;
      attributes_to_get: AttributeNameList.t ;
      limit: Integer.t option ;
      select: Select.t option ;
      scan_filter: FilterConditionMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      exclusive_start_key: Key.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      total_segments: Integer.t option ;
      segment: Integer.t option ;
      projection_expression: String.t option ;
      filter_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option ;
      consistent_read: Boolean.t option }
    let make ~table_name  ?index_name  ?(attributes_to_get= [])  ?limit 
      ?select  ?scan_filter  ?conditional_operator  ?exclusive_start_key 
      ?return_consumed_capacity  ?total_segments  ?segment 
      ?projection_expression  ?filter_expression  ?expression_attribute_names
       ?expression_attribute_values  ?consistent_read  () =
      {
        table_name;
        index_name;
        attributes_to_get;
        limit;
        select;
        scan_filter;
        conditional_operator;
        exclusive_start_key;
        return_consumed_capacity;
        total_segments;
        segment;
        projection_expression;
        filter_expression;
        expression_attribute_names;
        expression_attribute_values;
        consistent_read
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse));
          index_name =
            (Aws.Util.option_bind (Aws.Xml.member "IndexName" xml)
               String.parse);
          attributes_to_get =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "AttributesToGet" xml)
                  AttributeNameList.parse));
          limit =
            (Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse);
          select =
            (Aws.Util.option_bind (Aws.Xml.member "Select" xml) Select.parse);
          scan_filter =
            (Aws.Util.option_bind (Aws.Xml.member "ScanFilter" xml)
               FilterConditionMap.parse);
          conditional_operator =
            (Aws.Util.option_bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          exclusive_start_key =
            (Aws.Util.option_bind (Aws.Xml.member "ExclusiveStartKey" xml)
               Key.parse);
          return_consumed_capacity =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          total_segments =
            (Aws.Util.option_bind (Aws.Xml.member "TotalSegments" xml)
               Integer.parse);
          segment =
            (Aws.Util.option_bind (Aws.Xml.member "Segment" xml)
               Integer.parse);
          projection_expression =
            (Aws.Util.option_bind (Aws.Xml.member "ProjectionExpression" xml)
               String.parse);
          filter_expression =
            (Aws.Util.option_bind (Aws.Xml.member "FilterExpression" xml)
               String.parse);
          expression_attribute_names =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse);
          consistent_read =
            (Aws.Util.option_bind (Aws.Xml.member "ConsistentRead" xml)
               Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.consistent_read
              (fun f ->
                 Aws.Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Aws.Util.option_map v.expression_attribute_values
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeValues",
                    (ExpressionAttributeValueMap.to_query f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Aws.Util.option_map v.filter_expression
             (fun f ->
                Aws.Query.Pair ("FilterExpression", (String.to_query f)));
           Aws.Util.option_map v.projection_expression
             (fun f ->
                Aws.Query.Pair ("ProjectionExpression", (String.to_query f)));
           Aws.Util.option_map v.segment
             (fun f -> Aws.Query.Pair ("Segment", (Integer.to_query f)));
           Aws.Util.option_map v.total_segments
             (fun f -> Aws.Query.Pair ("TotalSegments", (Integer.to_query f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Aws.Util.option_map v.exclusive_start_key
             (fun f -> Aws.Query.Pair ("ExclusiveStartKey", (Key.to_query f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Aws.Util.option_map v.scan_filter
             (fun f ->
                Aws.Query.Pair
                  ("ScanFilter", (FilterConditionMap.to_query f)));
           Aws.Util.option_map v.select
             (fun f -> Aws.Query.Pair ("Select", (Select.to_query f)));
           Aws.Util.option_map v.limit
             (fun f -> Aws.Query.Pair ("Limit", (Integer.to_query f)));
           Some
             (Aws.Query.Pair
                ("AttributesToGet.member",
                  (AttributeNameList.to_query v.attributes_to_get)));
           Aws.Util.option_map v.index_name
             (fun f -> Aws.Query.Pair ("IndexName", (String.to_query f)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.consistent_read
              (fun f -> ("ConsistentRead", (Boolean.to_json f)));
           Aws.Util.option_map v.expression_attribute_values
             (fun f ->
                ("ExpressionAttributeValues",
                  (ExpressionAttributeValueMap.to_json f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)));
           Aws.Util.option_map v.filter_expression
             (fun f -> ("FilterExpression", (String.to_json f)));
           Aws.Util.option_map v.projection_expression
             (fun f -> ("ProjectionExpression", (String.to_json f)));
           Aws.Util.option_map v.segment
             (fun f -> ("Segment", (Integer.to_json f)));
           Aws.Util.option_map v.total_segments
             (fun f -> ("TotalSegments", (Integer.to_json f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)));
           Aws.Util.option_map v.exclusive_start_key
             (fun f -> ("ExclusiveStartKey", (Key.to_json f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)));
           Aws.Util.option_map v.scan_filter
             (fun f -> ("ScanFilter", (FilterConditionMap.to_json f)));
           Aws.Util.option_map v.select
             (fun f -> ("Select", (Select.to_json f)));
           Aws.Util.option_map v.limit
             (fun f -> ("Limit", (Integer.to_json f)));
           Some
             ("AttributesToGet",
               (AttributeNameList.to_json v.attributes_to_get));
           Aws.Util.option_map v.index_name
             (fun f -> ("IndexName", (String.to_json f)));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        index_name =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexName") String.of_json);
        attributes_to_get =
          (AttributeNameList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributesToGet")));
        limit =
          (Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json);
        select =
          (Aws.Util.option_map (Aws.Json.lookup j "Select") Select.of_json);
        scan_filter =
          (Aws.Util.option_map (Aws.Json.lookup j "ScanFilter")
             FilterConditionMap.of_json);
        conditional_operator =
          (Aws.Util.option_map (Aws.Json.lookup j "ConditionalOperator")
             ConditionalOperator.of_json);
        exclusive_start_key =
          (Aws.Util.option_map (Aws.Json.lookup j "ExclusiveStartKey")
             Key.of_json);
        return_consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnConsumedCapacity")
             ReturnConsumedCapacity.of_json);
        total_segments =
          (Aws.Util.option_map (Aws.Json.lookup j "TotalSegments")
             Integer.of_json);
        segment =
          (Aws.Util.option_map (Aws.Json.lookup j "Segment") Integer.of_json);
        projection_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "ProjectionExpression")
             String.of_json);
        filter_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "FilterExpression")
             String.of_json);
        expression_attribute_names =
          (Aws.Util.option_map (Aws.Json.lookup j "ExpressionAttributeNames")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ExpressionAttributeValues")
             ExpressionAttributeValueMap.of_json);
        consistent_read =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsistentRead")
             Boolean.of_json)
      }
  end
module BatchGetItemInput =
  struct
    type t =
      {
      request_items: BatchGetRequestMap.t ;
      return_consumed_capacity: ReturnConsumedCapacity.t option }
    let make ~request_items  ?return_consumed_capacity  () =
      { request_items; return_consumed_capacity }
    let parse xml =
      Some
        {
          request_items =
            (Aws.Xml.required "RequestItems"
               (Aws.Util.option_bind (Aws.Xml.member "RequestItems" xml)
                  BatchGetRequestMap.parse));
          return_consumed_capacity =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.return_consumed_capacity
              (fun f ->
                 Aws.Query.Pair
                   ("ReturnConsumedCapacity",
                     (ReturnConsumedCapacity.to_query f)));
           Some
             (Aws.Query.Pair
                ("RequestItems",
                  (BatchGetRequestMap.to_query v.request_items)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.return_consumed_capacity
              (fun f ->
                 ("ReturnConsumedCapacity",
                   (ReturnConsumedCapacity.to_json f)));
           Some
             ("RequestItems", (BatchGetRequestMap.to_json v.request_items))])
    let of_json j =
      {
        request_items =
          (BatchGetRequestMap.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "RequestItems")));
        return_consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnConsumedCapacity")
             ReturnConsumedCapacity.of_json)
      }
  end
module QueryInput =
  struct
    type t =
      {
      table_name: String.t ;
      index_name: String.t option ;
      select: Select.t option ;
      attributes_to_get: AttributeNameList.t ;
      limit: Integer.t option ;
      consistent_read: Boolean.t option ;
      key_conditions: KeyConditions.t option ;
      query_filter: FilterConditionMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      scan_index_forward: Boolean.t option ;
      exclusive_start_key: Key.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      projection_expression: String.t option ;
      filter_expression: String.t option ;
      key_condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ?index_name  ?select  ?(attributes_to_get= []) 
      ?limit  ?consistent_read  ?key_conditions  ?query_filter 
      ?conditional_operator  ?scan_index_forward  ?exclusive_start_key 
      ?return_consumed_capacity  ?projection_expression  ?filter_expression 
      ?key_condition_expression  ?expression_attribute_names 
      ?expression_attribute_values  () =
      {
        table_name;
        index_name;
        select;
        attributes_to_get;
        limit;
        consistent_read;
        key_conditions;
        query_filter;
        conditional_operator;
        scan_index_forward;
        exclusive_start_key;
        return_consumed_capacity;
        projection_expression;
        filter_expression;
        key_condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse));
          index_name =
            (Aws.Util.option_bind (Aws.Xml.member "IndexName" xml)
               String.parse);
          select =
            (Aws.Util.option_bind (Aws.Xml.member "Select" xml) Select.parse);
          attributes_to_get =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "AttributesToGet" xml)
                  AttributeNameList.parse));
          limit =
            (Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse);
          consistent_read =
            (Aws.Util.option_bind (Aws.Xml.member "ConsistentRead" xml)
               Boolean.parse);
          key_conditions =
            (Aws.Util.option_bind (Aws.Xml.member "KeyConditions" xml)
               KeyConditions.parse);
          query_filter =
            (Aws.Util.option_bind (Aws.Xml.member "QueryFilter" xml)
               FilterConditionMap.parse);
          conditional_operator =
            (Aws.Util.option_bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          scan_index_forward =
            (Aws.Util.option_bind (Aws.Xml.member "ScanIndexForward" xml)
               Boolean.parse);
          exclusive_start_key =
            (Aws.Util.option_bind (Aws.Xml.member "ExclusiveStartKey" xml)
               Key.parse);
          return_consumed_capacity =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          projection_expression =
            (Aws.Util.option_bind (Aws.Xml.member "ProjectionExpression" xml)
               String.parse);
          filter_expression =
            (Aws.Util.option_bind (Aws.Xml.member "FilterExpression" xml)
               String.parse);
          key_condition_expression =
            (Aws.Util.option_bind
               (Aws.Xml.member "KeyConditionExpression" xml) String.parse);
          expression_attribute_names =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_values
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Aws.Util.option_map v.key_condition_expression
             (fun f ->
                Aws.Query.Pair
                  ("KeyConditionExpression", (String.to_query f)));
           Aws.Util.option_map v.filter_expression
             (fun f ->
                Aws.Query.Pair ("FilterExpression", (String.to_query f)));
           Aws.Util.option_map v.projection_expression
             (fun f ->
                Aws.Query.Pair ("ProjectionExpression", (String.to_query f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Aws.Util.option_map v.exclusive_start_key
             (fun f -> Aws.Query.Pair ("ExclusiveStartKey", (Key.to_query f)));
           Aws.Util.option_map v.scan_index_forward
             (fun f ->
                Aws.Query.Pair ("ScanIndexForward", (Boolean.to_query f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Aws.Util.option_map v.query_filter
             (fun f ->
                Aws.Query.Pair
                  ("QueryFilter", (FilterConditionMap.to_query f)));
           Aws.Util.option_map v.key_conditions
             (fun f ->
                Aws.Query.Pair ("KeyConditions", (KeyConditions.to_query f)));
           Aws.Util.option_map v.consistent_read
             (fun f ->
                Aws.Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Aws.Util.option_map v.limit
             (fun f -> Aws.Query.Pair ("Limit", (Integer.to_query f)));
           Some
             (Aws.Query.Pair
                ("AttributesToGet.member",
                  (AttributeNameList.to_query v.attributes_to_get)));
           Aws.Util.option_map v.select
             (fun f -> Aws.Query.Pair ("Select", (Select.to_query f)));
           Aws.Util.option_map v.index_name
             (fun f -> Aws.Query.Pair ("IndexName", (String.to_query f)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_values
              (fun f ->
                 ("ExpressionAttributeValues",
                   (ExpressionAttributeValueMap.to_json f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)));
           Aws.Util.option_map v.key_condition_expression
             (fun f -> ("KeyConditionExpression", (String.to_json f)));
           Aws.Util.option_map v.filter_expression
             (fun f -> ("FilterExpression", (String.to_json f)));
           Aws.Util.option_map v.projection_expression
             (fun f -> ("ProjectionExpression", (String.to_json f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)));
           Aws.Util.option_map v.exclusive_start_key
             (fun f -> ("ExclusiveStartKey", (Key.to_json f)));
           Aws.Util.option_map v.scan_index_forward
             (fun f -> ("ScanIndexForward", (Boolean.to_json f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)));
           Aws.Util.option_map v.query_filter
             (fun f -> ("QueryFilter", (FilterConditionMap.to_json f)));
           Aws.Util.option_map v.key_conditions
             (fun f -> ("KeyConditions", (KeyConditions.to_json f)));
           Aws.Util.option_map v.consistent_read
             (fun f -> ("ConsistentRead", (Boolean.to_json f)));
           Aws.Util.option_map v.limit
             (fun f -> ("Limit", (Integer.to_json f)));
           Some
             ("AttributesToGet",
               (AttributeNameList.to_json v.attributes_to_get));
           Aws.Util.option_map v.select
             (fun f -> ("Select", (Select.to_json f)));
           Aws.Util.option_map v.index_name
             (fun f -> ("IndexName", (String.to_json f)));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        index_name =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexName") String.of_json);
        select =
          (Aws.Util.option_map (Aws.Json.lookup j "Select") Select.of_json);
        attributes_to_get =
          (AttributeNameList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributesToGet")));
        limit =
          (Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json);
        consistent_read =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsistentRead")
             Boolean.of_json);
        key_conditions =
          (Aws.Util.option_map (Aws.Json.lookup j "KeyConditions")
             KeyConditions.of_json);
        query_filter =
          (Aws.Util.option_map (Aws.Json.lookup j "QueryFilter")
             FilterConditionMap.of_json);
        conditional_operator =
          (Aws.Util.option_map (Aws.Json.lookup j "ConditionalOperator")
             ConditionalOperator.of_json);
        scan_index_forward =
          (Aws.Util.option_map (Aws.Json.lookup j "ScanIndexForward")
             Boolean.of_json);
        exclusive_start_key =
          (Aws.Util.option_map (Aws.Json.lookup j "ExclusiveStartKey")
             Key.of_json);
        return_consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnConsumedCapacity")
             ReturnConsumedCapacity.of_json);
        projection_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "ProjectionExpression")
             String.of_json);
        filter_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "FilterExpression")
             String.of_json);
        key_condition_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "KeyConditionExpression")
             String.of_json);
        expression_attribute_names =
          (Aws.Util.option_map (Aws.Json.lookup j "ExpressionAttributeNames")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ExpressionAttributeValues")
             ExpressionAttributeValueMap.of_json)
      }
  end
module DeleteTableInput =
  struct
    type t = {
      table_name: String.t }
    let make ~table_name  () = { table_name }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")))
      }
  end
module ScanOutput =
  struct
    type t =
      {
      items: ItemList.t ;
      count: Integer.t option ;
      scanned_count: Integer.t option ;
      last_evaluated_key: Key.t option ;
      consumed_capacity: ConsumedCapacity.t option }
    let make ?(items= [])  ?count  ?scanned_count  ?last_evaluated_key 
      ?consumed_capacity  () =
      { items; count; scanned_count; last_evaluated_key; consumed_capacity }
    let parse xml =
      Some
        {
          items =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "Items" xml)
                  ItemList.parse));
          count =
            (Aws.Util.option_bind (Aws.Xml.member "Count" xml) Integer.parse);
          scanned_count =
            (Aws.Util.option_bind (Aws.Xml.member "ScannedCount" xml)
               Integer.parse);
          last_evaluated_key =
            (Aws.Util.option_bind (Aws.Xml.member "LastEvaluatedKey" xml)
               Key.parse);
          consumed_capacity =
            (Aws.Util.option_bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.consumed_capacity
              (fun f ->
                 Aws.Query.Pair
                   ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Aws.Util.option_map v.last_evaluated_key
             (fun f -> Aws.Query.Pair ("LastEvaluatedKey", (Key.to_query f)));
           Aws.Util.option_map v.scanned_count
             (fun f -> Aws.Query.Pair ("ScannedCount", (Integer.to_query f)));
           Aws.Util.option_map v.count
             (fun f -> Aws.Query.Pair ("Count", (Integer.to_query f)));
           Some
             (Aws.Query.Pair ("Items.member", (ItemList.to_query v.items)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.consumed_capacity
              (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)));
           Aws.Util.option_map v.last_evaluated_key
             (fun f -> ("LastEvaluatedKey", (Key.to_json f)));
           Aws.Util.option_map v.scanned_count
             (fun f -> ("ScannedCount", (Integer.to_json f)));
           Aws.Util.option_map v.count
             (fun f -> ("Count", (Integer.to_json f)));
           Some ("Items", (ItemList.to_json v.items))])
    let of_json j =
      {
        items =
          (ItemList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Items")));
        count =
          (Aws.Util.option_map (Aws.Json.lookup j "Count") Integer.of_json);
        scanned_count =
          (Aws.Util.option_map (Aws.Json.lookup j "ScannedCount")
             Integer.of_json);
        last_evaluated_key =
          (Aws.Util.option_map (Aws.Json.lookup j "LastEvaluatedKey")
             Key.of_json);
        consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsumedCapacity")
             ConsumedCapacity.of_json)
      }
  end
module AttributeAction =
  struct
    type t =
      | ADD 
      | PUT 
      | DELETE 
    let str_to_t = [("DELETE", DELETE); ("PUT", PUT); ("ADD", ADD)]
    let t_to_str = [(DELETE, "DELETE"); (PUT, "PUT"); (ADD, "ADD")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AttributeValueUpdate =
  struct
    type t =
      {
      value: AttributeValue.t option ;
      action: AttributeAction.t option }
    let make ?value  ?action  () = { value; action }
    let parse xml =
      Some
        {
          value =
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml)
               AttributeValue.parse);
          action =
            (Aws.Util.option_bind (Aws.Xml.member "Action" xml)
               AttributeAction.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.action
              (fun f ->
                 Aws.Query.Pair ("Action", (AttributeAction.to_query f)));
           Aws.Util.option_map v.value
             (fun f -> Aws.Query.Pair ("Value", (AttributeValue.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.action
              (fun f -> ("Action", (AttributeAction.to_json f)));
           Aws.Util.option_map v.value
             (fun f -> ("Value", (AttributeValue.to_json f)))])
    let of_json j =
      {
        value =
          (Aws.Util.option_map (Aws.Json.lookup j "Value")
             AttributeValue.of_json);
        action =
          (Aws.Util.option_map (Aws.Json.lookup j "Action")
             AttributeAction.of_json)
      }
  end
module ConditionalCheckFailedException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      {
        message =
          (Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json)
      }
  end
module LocalSecondaryIndex =
  struct
    type t =
      {
      index_name: String.t ;
      key_schema: KeySchema.t ;
      projection: Projection.t }
    let make ~index_name  ~key_schema  ~projection  () =
      { index_name; key_schema; projection }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Aws.Util.option_bind (Aws.Xml.member "IndexName" xml)
                  String.parse));
          key_schema =
            (Aws.Xml.required "KeySchema"
               (Aws.Util.option_bind (Aws.Xml.member "KeySchema" xml)
                  KeySchema.parse));
          projection =
            (Aws.Xml.required "Projection"
               (Aws.Util.option_bind (Aws.Xml.member "Projection" xml)
                  Projection.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Projection", (Projection.to_query v.projection)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some
             (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Projection", (Projection.to_json v.projection));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")));
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        projection =
          (Projection.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Projection")))
      }
  end
module ItemCollectionKeyAttributeMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ItemCollectionMetrics =
  struct
    type t =
      {
      item_collection_key: ItemCollectionKeyAttributeMap.t option ;
      size_estimate_range_g_b: ItemCollectionSizeEstimateRange.t }
    let make ?item_collection_key  ?(size_estimate_range_g_b= [])  () =
      { item_collection_key; size_estimate_range_g_b }
    let parse xml =
      Some
        {
          item_collection_key =
            (Aws.Util.option_bind (Aws.Xml.member "ItemCollectionKey" xml)
               ItemCollectionKeyAttributeMap.parse);
          size_estimate_range_g_b =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "SizeEstimateRangeGB" xml)
                  ItemCollectionSizeEstimateRange.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("SizeEstimateRangeGB.member",
                   (ItemCollectionSizeEstimateRange.to_query
                      v.size_estimate_range_g_b)));
           Aws.Util.option_map v.item_collection_key
             (fun f ->
                Aws.Query.Pair
                  ("ItemCollectionKey",
                    (ItemCollectionKeyAttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("SizeEstimateRangeGB",
                (ItemCollectionSizeEstimateRange.to_json
                   v.size_estimate_range_g_b));
           Aws.Util.option_map v.item_collection_key
             (fun f ->
                ("ItemCollectionKey",
                  (ItemCollectionKeyAttributeMap.to_json f)))])
    let of_json j =
      {
        item_collection_key =
          (Aws.Util.option_map (Aws.Json.lookup j "ItemCollectionKey")
             ItemCollectionKeyAttributeMap.of_json);
        size_estimate_range_g_b =
          (ItemCollectionSizeEstimateRange.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "SizeEstimateRangeGB")))
      }
  end
module PutItemOutput =
  struct
    type t =
      {
      attributes: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option ;
      item_collection_metrics: ItemCollectionMetrics.t option }
    let make ?attributes  ?consumed_capacity  ?item_collection_metrics  () =
      { attributes; consumed_capacity; item_collection_metrics }
    let parse xml =
      Some
        {
          attributes =
            (Aws.Util.option_bind (Aws.Xml.member "Attributes" xml)
               AttributeMap.parse);
          consumed_capacity =
            (Aws.Util.option_bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse);
          item_collection_metrics =
            (Aws.Util.option_bind
               (Aws.Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetrics.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.item_collection_metrics
              (fun f ->
                 Aws.Query.Pair
                   ("ItemCollectionMetrics",
                     (ItemCollectionMetrics.to_query f)));
           Aws.Util.option_map v.consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Aws.Util.option_map v.attributes
             (fun f ->
                Aws.Query.Pair ("Attributes", (AttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.item_collection_metrics
              (fun f ->
                 ("ItemCollectionMetrics", (ItemCollectionMetrics.to_json f)));
           Aws.Util.option_map v.consumed_capacity
             (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)));
           Aws.Util.option_map v.attributes
             (fun f -> ("Attributes", (AttributeMap.to_json f)))])
    let of_json j =
      {
        attributes =
          (Aws.Util.option_map (Aws.Json.lookup j "Attributes")
             AttributeMap.of_json);
        consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsumedCapacity")
             ConsumedCapacity.of_json);
        item_collection_metrics =
          (Aws.Util.option_map (Aws.Json.lookup j "ItemCollectionMetrics")
             ItemCollectionMetrics.of_json)
      }
  end
module InternalServerError =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      {
        message =
          (Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json)
      }
  end
module GlobalSecondaryIndex =
  struct
    type t =
      {
      index_name: String.t ;
      key_schema: KeySchema.t ;
      projection: Projection.t ;
      provisioned_throughput: ProvisionedThroughput.t }
    let make ~index_name  ~key_schema  ~projection  ~provisioned_throughput 
      () = { index_name; key_schema; projection; provisioned_throughput }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Aws.Util.option_bind (Aws.Xml.member "IndexName" xml)
                  String.parse));
          key_schema =
            (Aws.Xml.required "KeySchema"
               (Aws.Util.option_bind (Aws.Xml.member "KeySchema" xml)
                  KeySchema.parse));
          projection =
            (Aws.Xml.required "Projection"
               (Aws.Util.option_bind (Aws.Xml.member "Projection" xml)
                  Projection.parse));
          provisioned_throughput =
            (Aws.Xml.required "ProvisionedThroughput"
               (Aws.Util.option_bind
                  (Aws.Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("ProvisionedThroughput",
                   (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Aws.Query.Pair
                ("Projection", (Projection.to_query v.projection)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some
             (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("ProvisionedThroughput",
                (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some ("Projection", (Projection.to_json v.projection));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")));
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        projection =
          (Projection.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Projection")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ProvisionedThroughput")))
      }
  end
module TableNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module ListTablesOutput =
  struct
    type t =
      {
      table_names: TableNameList.t ;
      last_evaluated_table_name: String.t option }
    let make ?(table_names= [])  ?last_evaluated_table_name  () =
      { table_names; last_evaluated_table_name }
    let parse xml =
      Some
        {
          table_names =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "TableNames" xml)
                  TableNameList.parse));
          last_evaluated_table_name =
            (Aws.Util.option_bind
               (Aws.Xml.member "LastEvaluatedTableName" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.last_evaluated_table_name
              (fun f ->
                 Aws.Query.Pair
                   ("LastEvaluatedTableName", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("TableNames.member", (TableNameList.to_query v.table_names)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.last_evaluated_table_name
              (fun f -> ("LastEvaluatedTableName", (String.to_json f)));
           Some ("TableNames", (TableNameList.to_json v.table_names))])
    let of_json j =
      {
        table_names =
          (TableNameList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableNames")));
        last_evaluated_table_name =
          (Aws.Util.option_map (Aws.Json.lookup j "LastEvaluatedTableName")
             String.of_json)
      }
  end
module GlobalSecondaryIndexList =
  struct
    type t = GlobalSecondaryIndex.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map GlobalSecondaryIndex.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list GlobalSecondaryIndex.to_query v
    let to_json v = `List (List.map GlobalSecondaryIndex.to_json v)
    let of_json j = Aws.Json.to_list GlobalSecondaryIndex.of_json j
  end
module CreateTableOutput =
  struct
    type t = {
      table_description: TableDescription.t option }
    let make ?table_description  () = { table_description }
    let parse xml =
      Some
        {
          table_description =
            (Aws.Util.option_bind (Aws.Xml.member "TableDescription" xml)
               TableDescription.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.table_description
              (fun f ->
                 Aws.Query.Pair
                   ("TableDescription", (TableDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.table_description
              (fun f -> ("TableDescription", (TableDescription.to_json f)))])
    let of_json j =
      {
        table_description =
          (Aws.Util.option_map (Aws.Json.lookup j "TableDescription")
             TableDescription.of_json)
      }
  end
module ItemCollectionMetricsMultiple =
  struct
    type t = ItemCollectionMetrics.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map ItemCollectionMetrics.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ItemCollectionMetrics.to_query v
    let to_json v = `List (List.map ItemCollectionMetrics.to_json v)
    let of_json j = Aws.Json.to_list ItemCollectionMetrics.of_json j
  end
module LocalSecondaryIndexList =
  struct
    type t = LocalSecondaryIndex.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map LocalSecondaryIndex.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list LocalSecondaryIndex.to_query v
    let to_json v = `List (List.map LocalSecondaryIndex.to_json v)
    let of_json j = Aws.Json.to_list LocalSecondaryIndex.of_json j
  end
module AttributeUpdates =
  struct
    type t = (String.t, AttributeValueUpdate.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string
        AttributeValueUpdate.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (AttributeValueUpdate.to_json v)) ::
                  acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValueUpdate.of_json j
  end
module BatchWriteItemInput =
  struct
    type t =
      {
      request_items: BatchWriteItemRequestMap.t ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option }
    let make ~request_items  ?return_consumed_capacity 
      ?return_item_collection_metrics  () =
      {
        request_items;
        return_consumed_capacity;
        return_item_collection_metrics
      }
    let parse xml =
      Some
        {
          request_items =
            (Aws.Xml.required "RequestItems"
               (Aws.Util.option_bind (Aws.Xml.member "RequestItems" xml)
                  BatchWriteItemRequestMap.parse));
          return_consumed_capacity =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.return_item_collection_metrics
              (fun f ->
                 Aws.Query.Pair
                   ("ReturnItemCollectionMetrics",
                     (ReturnItemCollectionMetrics.to_query f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Some
             (Aws.Query.Pair
                ("RequestItems",
                  (BatchWriteItemRequestMap.to_query v.request_items)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.return_item_collection_metrics
              (fun f ->
                 ("ReturnItemCollectionMetrics",
                   (ReturnItemCollectionMetrics.to_json f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)));
           Some
             ("RequestItems",
               (BatchWriteItemRequestMap.to_json v.request_items))])
    let of_json j =
      {
        request_items =
          (BatchWriteItemRequestMap.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "RequestItems")));
        return_consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnConsumedCapacity")
             ReturnConsumedCapacity.of_json);
        return_item_collection_metrics =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ReturnItemCollectionMetrics")
             ReturnItemCollectionMetrics.of_json)
      }
  end
module DescribeTableInput =
  struct
    type t = {
      table_name: String.t }
    let make ~table_name  () = { table_name }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")))
      }
  end
module CreateTableInput =
  struct
    type t =
      {
      attribute_definitions: AttributeDefinitions.t ;
      table_name: String.t ;
      key_schema: KeySchema.t ;
      local_secondary_indexes: LocalSecondaryIndexList.t ;
      global_secondary_indexes: GlobalSecondaryIndexList.t ;
      provisioned_throughput: ProvisionedThroughput.t ;
      stream_specification: StreamSpecification.t option }
    let make ~attribute_definitions  ~table_name  ~key_schema 
      ?(local_secondary_indexes= [])  ?(global_secondary_indexes= []) 
      ~provisioned_throughput  ?stream_specification  () =
      {
        attribute_definitions;
        table_name;
        key_schema;
        local_secondary_indexes;
        global_secondary_indexes;
        provisioned_throughput;
        stream_specification
      }
    let parse xml =
      Some
        {
          attribute_definitions =
            (Aws.Xml.required "AttributeDefinitions"
               (Aws.Util.option_bind
                  (Aws.Xml.member "AttributeDefinitions" xml)
                  AttributeDefinitions.parse));
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse));
          key_schema =
            (Aws.Xml.required "KeySchema"
               (Aws.Util.option_bind (Aws.Xml.member "KeySchema" xml)
                  KeySchema.parse));
          local_secondary_indexes =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "LocalSecondaryIndexes" xml)
                  LocalSecondaryIndexList.parse));
          global_secondary_indexes =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "GlobalSecondaryIndexes" xml)
                  GlobalSecondaryIndexList.parse));
          provisioned_throughput =
            (Aws.Xml.required "ProvisionedThroughput"
               (Aws.Util.option_bind
                  (Aws.Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse));
          stream_specification =
            (Aws.Util.option_bind (Aws.Xml.member "StreamSpecification" xml)
               StreamSpecification.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.stream_specification
              (fun f ->
                 Aws.Query.Pair
                   ("StreamSpecification", (StreamSpecification.to_query f)));
           Some
             (Aws.Query.Pair
                ("ProvisionedThroughput",
                  (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Aws.Query.Pair
                ("GlobalSecondaryIndexes.member",
                  (GlobalSecondaryIndexList.to_query
                     v.global_secondary_indexes)));
           Some
             (Aws.Query.Pair
                ("LocalSecondaryIndexes.member",
                  (LocalSecondaryIndexList.to_query v.local_secondary_indexes)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)));
           Some
             (Aws.Query.Pair
                ("AttributeDefinitions.member",
                  (AttributeDefinitions.to_query v.attribute_definitions)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.stream_specification
              (fun f ->
                 ("StreamSpecification", (StreamSpecification.to_json f)));
           Some
             ("ProvisionedThroughput",
               (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some
             ("GlobalSecondaryIndexes",
               (GlobalSecondaryIndexList.to_json v.global_secondary_indexes));
           Some
             ("LocalSecondaryIndexes",
               (LocalSecondaryIndexList.to_json v.local_secondary_indexes));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Some ("TableName", (String.to_json v.table_name));
           Some
             ("AttributeDefinitions",
               (AttributeDefinitions.to_json v.attribute_definitions))])
    let of_json j =
      {
        attribute_definitions =
          (AttributeDefinitions.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "AttributeDefinitions")));
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        local_secondary_indexes =
          (LocalSecondaryIndexList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "LocalSecondaryIndexes")));
        global_secondary_indexes =
          (GlobalSecondaryIndexList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "GlobalSecondaryIndexes")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ProvisionedThroughput")));
        stream_specification =
          (Aws.Util.option_map (Aws.Json.lookup j "StreamSpecification")
             StreamSpecification.of_json)
      }
  end
module ResourceInUseException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      {
        message =
          (Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json)
      }
  end
module UpdateItemOutput =
  struct
    type t =
      {
      attributes: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option ;
      item_collection_metrics: ItemCollectionMetrics.t option }
    let make ?attributes  ?consumed_capacity  ?item_collection_metrics  () =
      { attributes; consumed_capacity; item_collection_metrics }
    let parse xml =
      Some
        {
          attributes =
            (Aws.Util.option_bind (Aws.Xml.member "Attributes" xml)
               AttributeMap.parse);
          consumed_capacity =
            (Aws.Util.option_bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse);
          item_collection_metrics =
            (Aws.Util.option_bind
               (Aws.Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetrics.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.item_collection_metrics
              (fun f ->
                 Aws.Query.Pair
                   ("ItemCollectionMetrics",
                     (ItemCollectionMetrics.to_query f)));
           Aws.Util.option_map v.consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Aws.Util.option_map v.attributes
             (fun f ->
                Aws.Query.Pair ("Attributes", (AttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.item_collection_metrics
              (fun f ->
                 ("ItemCollectionMetrics", (ItemCollectionMetrics.to_json f)));
           Aws.Util.option_map v.consumed_capacity
             (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)));
           Aws.Util.option_map v.attributes
             (fun f -> ("Attributes", (AttributeMap.to_json f)))])
    let of_json j =
      {
        attributes =
          (Aws.Util.option_map (Aws.Json.lookup j "Attributes")
             AttributeMap.of_json);
        consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsumedCapacity")
             ConsumedCapacity.of_json);
        item_collection_metrics =
          (Aws.Util.option_map (Aws.Json.lookup j "ItemCollectionMetrics")
             ItemCollectionMetrics.of_json)
      }
  end
module GetItemOutput =
  struct
    type t =
      {
      item: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option }
    let make ?item  ?consumed_capacity  () = { item; consumed_capacity }
    let parse xml =
      Some
        {
          item =
            (Aws.Util.option_bind (Aws.Xml.member "Item" xml)
               AttributeMap.parse);
          consumed_capacity =
            (Aws.Util.option_bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.consumed_capacity
              (fun f ->
                 Aws.Query.Pair
                   ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Aws.Util.option_map v.item
             (fun f -> Aws.Query.Pair ("Item", (AttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.consumed_capacity
              (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)));
           Aws.Util.option_map v.item
             (fun f -> ("Item", (AttributeMap.to_json f)))])
    let of_json j =
      {
        item =
          (Aws.Util.option_map (Aws.Json.lookup j "Item")
             AttributeMap.of_json);
        consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsumedCapacity")
             ConsumedCapacity.of_json)
      }
  end
module GetItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      key: Key.t ;
      attributes_to_get: AttributeNameList.t ;
      consistent_read: Boolean.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      projection_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option }
    let make ~table_name  ~key  ?(attributes_to_get= [])  ?consistent_read 
      ?return_consumed_capacity  ?projection_expression 
      ?expression_attribute_names  () =
      {
        table_name;
        key;
        attributes_to_get;
        consistent_read;
        return_consumed_capacity;
        projection_expression;
        expression_attribute_names
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) Key.parse));
          attributes_to_get =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "AttributesToGet" xml)
                  AttributeNameList.parse));
          consistent_read =
            (Aws.Util.option_bind (Aws.Xml.member "ConsistentRead" xml)
               Boolean.parse);
          return_consumed_capacity =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          projection_expression =
            (Aws.Util.option_bind (Aws.Xml.member "ProjectionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_names
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeNames",
                     (ExpressionAttributeNameMap.to_query f)));
           Aws.Util.option_map v.projection_expression
             (fun f ->
                Aws.Query.Pair ("ProjectionExpression", (String.to_query f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Aws.Util.option_map v.consistent_read
             (fun f ->
                Aws.Query.Pair ("ConsistentRead", (Boolean.to_query f)));
           Some
             (Aws.Query.Pair
                ("AttributesToGet.member",
                  (AttributeNameList.to_query v.attributes_to_get)));
           Some (Aws.Query.Pair ("Key", (Key.to_query v.key)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_names
              (fun f ->
                 ("ExpressionAttributeNames",
                   (ExpressionAttributeNameMap.to_json f)));
           Aws.Util.option_map v.projection_expression
             (fun f -> ("ProjectionExpression", (String.to_json f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)));
           Aws.Util.option_map v.consistent_read
             (fun f -> ("ConsistentRead", (Boolean.to_json f)));
           Some
             ("AttributesToGet",
               (AttributeNameList.to_json v.attributes_to_get));
           Some ("Key", (Key.to_json v.key));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        key =
          (Key.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        attributes_to_get =
          (AttributeNameList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributesToGet")));
        consistent_read =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsistentRead")
             Boolean.of_json);
        return_consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnConsumedCapacity")
             ReturnConsumedCapacity.of_json);
        projection_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "ProjectionExpression")
             String.of_json);
        expression_attribute_names =
          (Aws.Util.option_map (Aws.Json.lookup j "ExpressionAttributeNames")
             ExpressionAttributeNameMap.of_json)
      }
  end
module DeleteItemOutput =
  struct
    type t =
      {
      attributes: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option ;
      item_collection_metrics: ItemCollectionMetrics.t option }
    let make ?attributes  ?consumed_capacity  ?item_collection_metrics  () =
      { attributes; consumed_capacity; item_collection_metrics }
    let parse xml =
      Some
        {
          attributes =
            (Aws.Util.option_bind (Aws.Xml.member "Attributes" xml)
               AttributeMap.parse);
          consumed_capacity =
            (Aws.Util.option_bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse);
          item_collection_metrics =
            (Aws.Util.option_bind
               (Aws.Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetrics.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.item_collection_metrics
              (fun f ->
                 Aws.Query.Pair
                   ("ItemCollectionMetrics",
                     (ItemCollectionMetrics.to_query f)));
           Aws.Util.option_map v.consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Aws.Util.option_map v.attributes
             (fun f ->
                Aws.Query.Pair ("Attributes", (AttributeMap.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.item_collection_metrics
              (fun f ->
                 ("ItemCollectionMetrics", (ItemCollectionMetrics.to_json f)));
           Aws.Util.option_map v.consumed_capacity
             (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)));
           Aws.Util.option_map v.attributes
             (fun f -> ("Attributes", (AttributeMap.to_json f)))])
    let of_json j =
      {
        attributes =
          (Aws.Util.option_map (Aws.Json.lookup j "Attributes")
             AttributeMap.of_json);
        consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsumedCapacity")
             ConsumedCapacity.of_json);
        item_collection_metrics =
          (Aws.Util.option_map (Aws.Json.lookup j "ItemCollectionMetrics")
             ItemCollectionMetrics.of_json)
      }
  end
module DeleteTableOutput =
  struct
    type t = {
      table_description: TableDescription.t option }
    let make ?table_description  () = { table_description }
    let parse xml =
      Some
        {
          table_description =
            (Aws.Util.option_bind (Aws.Xml.member "TableDescription" xml)
               TableDescription.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.table_description
              (fun f ->
                 Aws.Query.Pair
                   ("TableDescription", (TableDescription.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.table_description
              (fun f -> ("TableDescription", (TableDescription.to_json f)))])
    let of_json j =
      {
        table_description =
          (Aws.Util.option_map (Aws.Json.lookup j "TableDescription")
             TableDescription.of_json)
      }
  end
module ListTablesInput =
  struct
    type t =
      {
      exclusive_start_table_name: String.t option ;
      limit: Integer.t option }
    let make ?exclusive_start_table_name  ?limit  () =
      { exclusive_start_table_name; limit }
    let parse xml =
      Some
        {
          exclusive_start_table_name =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExclusiveStartTableName" xml) String.parse);
          limit =
            (Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.limit
              (fun f -> Aws.Query.Pair ("Limit", (Integer.to_query f)));
           Aws.Util.option_map v.exclusive_start_table_name
             (fun f ->
                Aws.Query.Pair
                  ("ExclusiveStartTableName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.limit
              (fun f -> ("Limit", (Integer.to_json f)));
           Aws.Util.option_map v.exclusive_start_table_name
             (fun f -> ("ExclusiveStartTableName", (String.to_json f)))])
    let of_json j =
      {
        exclusive_start_table_name =
          (Aws.Util.option_map (Aws.Json.lookup j "ExclusiveStartTableName")
             String.of_json);
        limit =
          (Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json)
      }
  end
module ItemCollectionMetricsPerTable =
  struct
    type t = (String.t, ItemCollectionMetricsMultiple.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string
        ItemCollectionMetricsMultiple.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k),
                    (ItemCollectionMetricsMultiple.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string
        ItemCollectionMetricsMultiple.of_json j
  end
module BatchWriteItemOutput =
  struct
    type t =
      {
      unprocessed_items: BatchWriteItemRequestMap.t option ;
      item_collection_metrics: ItemCollectionMetricsPerTable.t option ;
      consumed_capacity: ConsumedCapacityMultiple.t }
    let make ?unprocessed_items  ?item_collection_metrics 
      ?(consumed_capacity= [])  () =
      { unprocessed_items; item_collection_metrics; consumed_capacity }
    let parse xml =
      Some
        {
          unprocessed_items =
            (Aws.Util.option_bind (Aws.Xml.member "UnprocessedItems" xml)
               BatchWriteItemRequestMap.parse);
          item_collection_metrics =
            (Aws.Util.option_bind
               (Aws.Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetricsPerTable.parse);
          consumed_capacity =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "ConsumedCapacity" xml)
                  ConsumedCapacityMultiple.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("ConsumedCapacity.member",
                   (ConsumedCapacityMultiple.to_query v.consumed_capacity)));
           Aws.Util.option_map v.item_collection_metrics
             (fun f ->
                Aws.Query.Pair
                  ("ItemCollectionMetrics",
                    (ItemCollectionMetricsPerTable.to_query f)));
           Aws.Util.option_map v.unprocessed_items
             (fun f ->
                Aws.Query.Pair
                  ("UnprocessedItems", (BatchWriteItemRequestMap.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("ConsumedCapacity",
                (ConsumedCapacityMultiple.to_json v.consumed_capacity));
           Aws.Util.option_map v.item_collection_metrics
             (fun f ->
                ("ItemCollectionMetrics",
                  (ItemCollectionMetricsPerTable.to_json f)));
           Aws.Util.option_map v.unprocessed_items
             (fun f ->
                ("UnprocessedItems", (BatchWriteItemRequestMap.to_json f)))])
    let of_json j =
      {
        unprocessed_items =
          (Aws.Util.option_map (Aws.Json.lookup j "UnprocessedItems")
             BatchWriteItemRequestMap.of_json);
        item_collection_metrics =
          (Aws.Util.option_map (Aws.Json.lookup j "ItemCollectionMetrics")
             ItemCollectionMetricsPerTable.of_json);
        consumed_capacity =
          (ConsumedCapacityMultiple.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ConsumedCapacity")))
      }
  end
module UpdateItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      key: Key.t ;
      attribute_updates: AttributeUpdates.t option ;
      expected: ExpectedAttributeMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      return_values: ReturnValue.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option ;
      update_expression: String.t option ;
      condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ~key  ?attribute_updates  ?expected 
      ?conditional_operator  ?return_values  ?return_consumed_capacity 
      ?return_item_collection_metrics  ?update_expression 
      ?condition_expression  ?expression_attribute_names 
      ?expression_attribute_values  () =
      {
        table_name;
        key;
        attribute_updates;
        expected;
        conditional_operator;
        return_values;
        return_consumed_capacity;
        return_item_collection_metrics;
        update_expression;
        condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Aws.Util.option_bind (Aws.Xml.member "TableName" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) Key.parse));
          attribute_updates =
            (Aws.Util.option_bind (Aws.Xml.member "AttributeUpdates" xml)
               AttributeUpdates.parse);
          expected =
            (Aws.Util.option_bind (Aws.Xml.member "Expected" xml)
               ExpectedAttributeMap.parse);
          conditional_operator =
            (Aws.Util.option_bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          return_values =
            (Aws.Util.option_bind (Aws.Xml.member "ReturnValues" xml)
               ReturnValue.parse);
          return_consumed_capacity =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse);
          update_expression =
            (Aws.Util.option_bind (Aws.Xml.member "UpdateExpression" xml)
               String.parse);
          condition_expression =
            (Aws.Util.option_bind (Aws.Xml.member "ConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_values
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)));
           Aws.Util.option_map v.condition_expression
             (fun f ->
                Aws.Query.Pair ("ConditionExpression", (String.to_query f)));
           Aws.Util.option_map v.update_expression
             (fun f ->
                Aws.Query.Pair ("UpdateExpression", (String.to_query f)));
           Aws.Util.option_map v.return_item_collection_metrics
             (fun f ->
                Aws.Query.Pair
                  ("ReturnItemCollectionMetrics",
                    (ReturnItemCollectionMetrics.to_query f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)));
           Aws.Util.option_map v.return_values
             (fun f ->
                Aws.Query.Pair ("ReturnValues", (ReturnValue.to_query f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)));
           Aws.Util.option_map v.expected
             (fun f ->
                Aws.Query.Pair
                  ("Expected", (ExpectedAttributeMap.to_query f)));
           Aws.Util.option_map v.attribute_updates
             (fun f ->
                Aws.Query.Pair
                  ("AttributeUpdates", (AttributeUpdates.to_query f)));
           Some (Aws.Query.Pair ("Key", (Key.to_query v.key)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expression_attribute_values
              (fun f ->
                 ("ExpressionAttributeValues",
                   (ExpressionAttributeValueMap.to_json f)));
           Aws.Util.option_map v.expression_attribute_names
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)));
           Aws.Util.option_map v.condition_expression
             (fun f -> ("ConditionExpression", (String.to_json f)));
           Aws.Util.option_map v.update_expression
             (fun f -> ("UpdateExpression", (String.to_json f)));
           Aws.Util.option_map v.return_item_collection_metrics
             (fun f ->
                ("ReturnItemCollectionMetrics",
                  (ReturnItemCollectionMetrics.to_json f)));
           Aws.Util.option_map v.return_consumed_capacity
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)));
           Aws.Util.option_map v.return_values
             (fun f -> ("ReturnValues", (ReturnValue.to_json f)));
           Aws.Util.option_map v.conditional_operator
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)));
           Aws.Util.option_map v.expected
             (fun f -> ("Expected", (ExpectedAttributeMap.to_json f)));
           Aws.Util.option_map v.attribute_updates
             (fun f -> ("AttributeUpdates", (AttributeUpdates.to_json f)));
           Some ("Key", (Key.to_json v.key));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        key =
          (Key.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        attribute_updates =
          (Aws.Util.option_map (Aws.Json.lookup j "AttributeUpdates")
             AttributeUpdates.of_json);
        expected =
          (Aws.Util.option_map (Aws.Json.lookup j "Expected")
             ExpectedAttributeMap.of_json);
        conditional_operator =
          (Aws.Util.option_map (Aws.Json.lookup j "ConditionalOperator")
             ConditionalOperator.of_json);
        return_values =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnValues")
             ReturnValue.of_json);
        return_consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ReturnConsumedCapacity")
             ReturnConsumedCapacity.of_json);
        return_item_collection_metrics =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ReturnItemCollectionMetrics")
             ReturnItemCollectionMetrics.of_json);
        update_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "UpdateExpression")
             String.of_json);
        condition_expression =
          (Aws.Util.option_map (Aws.Json.lookup j "ConditionExpression")
             String.of_json);
        expression_attribute_names =
          (Aws.Util.option_map (Aws.Json.lookup j "ExpressionAttributeNames")
             ExpressionAttributeNameMap.of_json);
        expression_attribute_values =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ExpressionAttributeValues")
             ExpressionAttributeValueMap.of_json)
      }
  end
module QueryOutput =
  struct
    type t =
      {
      items: ItemList.t ;
      count: Integer.t option ;
      scanned_count: Integer.t option ;
      last_evaluated_key: Key.t option ;
      consumed_capacity: ConsumedCapacity.t option }
    let make ?(items= [])  ?count  ?scanned_count  ?last_evaluated_key 
      ?consumed_capacity  () =
      { items; count; scanned_count; last_evaluated_key; consumed_capacity }
    let parse xml =
      Some
        {
          items =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "Items" xml)
                  ItemList.parse));
          count =
            (Aws.Util.option_bind (Aws.Xml.member "Count" xml) Integer.parse);
          scanned_count =
            (Aws.Util.option_bind (Aws.Xml.member "ScannedCount" xml)
               Integer.parse);
          last_evaluated_key =
            (Aws.Util.option_bind (Aws.Xml.member "LastEvaluatedKey" xml)
               Key.parse);
          consumed_capacity =
            (Aws.Util.option_bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.consumed_capacity
              (fun f ->
                 Aws.Query.Pair
                   ("ConsumedCapacity", (ConsumedCapacity.to_query f)));
           Aws.Util.option_map v.last_evaluated_key
             (fun f -> Aws.Query.Pair ("LastEvaluatedKey", (Key.to_query f)));
           Aws.Util.option_map v.scanned_count
             (fun f -> Aws.Query.Pair ("ScannedCount", (Integer.to_query f)));
           Aws.Util.option_map v.count
             (fun f -> Aws.Query.Pair ("Count", (Integer.to_query f)));
           Some
             (Aws.Query.Pair ("Items.member", (ItemList.to_query v.items)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.consumed_capacity
              (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)));
           Aws.Util.option_map v.last_evaluated_key
             (fun f -> ("LastEvaluatedKey", (Key.to_json f)));
           Aws.Util.option_map v.scanned_count
             (fun f -> ("ScannedCount", (Integer.to_json f)));
           Aws.Util.option_map v.count
             (fun f -> ("Count", (Integer.to_json f)));
           Some ("Items", (ItemList.to_json v.items))])
    let of_json j =
      {
        items =
          (ItemList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Items")));
        count =
          (Aws.Util.option_map (Aws.Json.lookup j "Count") Integer.of_json);
        scanned_count =
          (Aws.Util.option_map (Aws.Json.lookup j "ScannedCount")
             Integer.of_json);
        last_evaluated_key =
          (Aws.Util.option_map (Aws.Json.lookup j "LastEvaluatedKey")
             Key.of_json);
        consumed_capacity =
          (Aws.Util.option_map (Aws.Json.lookup j "ConsumedCapacity")
             ConsumedCapacity.of_json)
      }
  end