class zplanned_change definition
  public
  create protected .

  public section.

    types:
      ty_t_keys type standard table of cdpos_uid-tabkey with default key .

    constants action_update type cdchngind value 'U' ##NO_TEXT.
    constants action_insert type cdchngind value 'I' ##NO_TEXT.
    constants action_delete type cdchngind value 'D' ##NO_TEXT.
    constants action_delete_single_field type cdchngind value 'E' ##NO_TEXT.
    constants action_insert_single_field type cdchngind value 'J' ##NO_TEXT.

    class-methods create
      importing
        value(objectclas) type cdobjectcl
        value(objectid)   type clike
        value(tcode)      type cdtcode default sy-tcode
        value(udate)      type cddatum optional
        value(utime)      type cduzeit optional
      returning
        value(instance)   type ref to zplanned_change
      raising
        zcx_planned_change_error .
    class-methods load
      importing
        value(objectclas) type cdobjectcl
        value(objectid)   type cdobjectv
        value(changenr)   type cdchangenr
      returning
        value(instance)   type ref to zplanned_change
      raising
        zcx_planned_change_error .
    methods save
      returning
        value(changenr) type cdchangenr
      raising
        zcx_planned_change_error .
    methods apply
      raising
        zcx_planned_change_error .
    class-methods clear_global_data
      importing
        value(repid) type syrepid .
    methods delete
      raising
        zcx_planned_change_error .
    methods add_table_line
      importing
        !new                    type any optional
        !old                    type any optional
        value(change_indicator) type cdchngind default action_insert
      raising
        zcx_planned_change_error .
    methods add_table
      importing
        !new                    type table optional
        !old                    type table optional
        value(change_indicator) type cdchngind default action_insert
      raising
        zcx_planned_change_error .
    methods get_table
      importing
        value(table_name) type tabname
      changing
        !table            type table
      raising
        zcx_planned_change_error .
    methods get_table_line
      importing
        value(table_name) type tabname
        value(key)        type cdpos_uid-tabkey
      changing
        !line             type any
      raising
        zcx_planned_change_error .
    methods get_table_keys
      importing
        value(table_name) type tabname
      returning
        value(keys)       type ty_t_keys .
  protected section.

    data gs_cdhdr type cdhdr .
    data ins_cdpos type cdpos_tab .
    data:
      gt_cdpos_uid type table of cdpos_uid .
    data gt_cdpos_str type cdpos_str_tab .

    methods constructor
      importing
        value(objectclas) type cdobjectcl
        value(objectid)   type cdobjectv
        value(changenr)   type cdchangenr optional
        value(tcode)      type cdtcode optional
        value(udate)      type cddatum optional
        value(utime)      type cduzeit optional
      raising
        zcx_planned_change_error .
    methods apply_table
      importing
        value(table_name) type tabname
      raising
        zcx_planned_change_error .
    methods apply_table_line
      importing
        value(table_name) type tabname
        !key              type cdpos_uid-tabkey
      raising
        zcx_planned_change_error .
    methods sync_me_to_scd0 .
    methods sync_scd0_to_me .
  private section.
ENDCLASS.



CLASS ZPLANNED_CHANGE IMPLEMENTATION.


  method add_table.
    data: lo_tabledescr     type ref to cl_abap_tabledescr,
          lo_structdescr    type ref to cl_abap_structdescr,
          lt_kz_components  type cl_abap_structdescr=>component_table,
          lo_structdescr_kz type ref to cl_abap_structdescr,
          lo_tabledescr_kz  type ref to cl_abap_tabledescr,
          ld_new_kz         type ref to data,
          ld_old_kz         type ref to data,
          lv_tabname        type tabname,
          lv_msg            type string.

    field-symbols: <new_kz>    type table,
                   <old_kz>    type table,
                   <line>      type any,
                   <line_kz>   type any,
                   <component> like line of lt_kz_components.

    "// Descobre nome e campos da tabela
    if new is not initial.
      lo_tabledescr ?= cl_abap_structdescr=>describe_by_data( new ).
    elseif old is not initial.
      lo_tabledescr ?= cl_abap_structdescr=>describe_by_data( old ).
    else.
      raise exception type zcx_planned_change_error
        exporting
          textid     = zcx_planned_change_error=>invalid_parameter
          param_name = 'NEW/OLD'.
    endif.
    if lo_tabledescr->type_kind na cl_abap_typedescr=>typekind_table.
      raise exception type zcx_planned_change_error
        exporting
          textid     = zcx_planned_change_error=>invalid_parameter
          param_name = 'NEW'.
    endif.
    lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
    lv_tabname = lo_structdescr->get_relative_name( ).

    "// Create internal tables with KZ field
    lt_kz_components = lo_structdescr->get_components( ).
    append initial line to lt_kz_components assigning <component>.
    <component>-name = 'KZ'.
    <component>-type ?= cl_abap_datadescr=>describe_by_name( 'CHAR1' ).
    lo_structdescr_kz = cl_abap_structdescr=>create( lt_kz_components ).
    lo_tabledescr_kz = cl_abap_tabledescr=>create( lo_structdescr_kz ).
    create data ld_new_kz type handle lo_tabledescr_kz.
    assign ld_new_kz->* to <new_kz>.
    create data ld_old_kz type handle lo_tabledescr.
    assign ld_old_kz->* to <old_kz>.

    "// Build KZ tables
    loop at new assigning <line>.
      append initial line to <new_kz> assigning <line_kz>.
      move-corresponding <line> to <line_kz>.
    endloop.
    loop at old assigning <line>.
      append initial line to <old_kz> assigning <line_kz>.
      move-corresponding <line> to <line_kz>.
    endloop.

    "// Add change to cache
    call function 'CHANGEDOCUMENT_MULTIPLE_CASE2'
      exporting
        change_indicator       = change_indicator
        tablename              = lv_tabname
        table_new              = <new_kz>
        table_old              = <old_kz>
      exceptions
        nametab_error          = 1
        open_missing           = 2
        position_insert_failed = 3
        others                 = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              into lv_msg.
      raise exception type zcx_planned_change_error
        exporting
          textid = zcx_planned_change_error=>error_message
          msg    = lv_msg.
    endif.
  endmethod.


  method add_table_line.
    data: lo_structdescr type ref to cl_abap_structdescr,
          lv_tabname     type tabname,
          lv_msg         type string.

    "// Find out table's name and fields
    if new is not initial.
      lo_structdescr ?= cl_abap_structdescr=>describe_by_data( new ).
    elseif old is not initial.
      lo_structdescr ?= cl_abap_structdescr=>describe_by_data( old ).
    else.
      raise exception type zcx_planned_change_error
        exporting
          textid     = zcx_planned_change_error=>invalid_parameter
          param_name = 'NEW/OLD'.
    endif.
    if lo_structdescr->type_kind na 'uv'. "// typekind_struct*
      raise exception type zcx_planned_change_error
        exporting
          textid     = zcx_planned_change_error=>invalid_parameter
          param_name = 'NEW/OLD'.
    endif.
    lv_tabname = lo_structdescr->get_relative_name( ).

    "// Add change to cache
    call function 'CHANGEDOCUMENT_SINGLE_CASE'
      exporting
        change_indicator       = change_indicator
        tablename              = lv_tabname
        workarea_new           = new
        workarea_old           = old
      exceptions
        nametab_error          = 1
        open_missing           = 2
        position_insert_failed = 3
        others                 = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              into lv_msg.
      raise exception type zcx_planned_change_error
        exporting
          textid = zcx_planned_change_error=>error_message
          msg    = lv_msg.
    endif.
  endmethod.


  method apply.
    data: lt_tables type table of tabname.

    field-symbols: <cdpos> like line of me->ins_cdpos,
                   <table> like line of lt_tables,
                   <line>  type any.

    "// Group tables
    loop at me->ins_cdpos assigning <cdpos>.
      append <cdpos>-tabname to lt_tables.
    endloop.
    sort lt_tables. delete adjacent duplicates from lt_tables.

    "// Process each table
    loop at lt_tables assigning <table>.
      apply_table( <table> ).
    endloop.

    "// Delete Change Document
    delete( ).
  endmethod.


  method apply_table.
    data: ld_table type ref to data,
          lt_keys  type ty_t_keys.

    field-symbols: <cdpos> like line of me->ins_cdpos,
                   <table> type table,
                   <key>   like line of lt_keys.

    "// Create work area for the table
    create data ld_table type standard table of (table_name).
    assign ld_table->* to <table>.

    "// Process each line
    lt_keys = get_table_keys( table_name ).
    loop at lt_keys assigning <key>.
      apply_table_line(
        table_name = table_name
        key = <key>
      ).
    endloop.

    free ld_table.
  endmethod.


  method apply_table_line.
    data: ld_line       type ref to data,
          lv_operation  type cdchngind value action_insert,
          lt_key_fields type table of fieldname,
          lt_key_conds  type table of string,
          lv_where_cond type string,
          lv_msg        type string.

    field-symbols: <cdpos>     like line of me->ins_cdpos,
                   <key_field> like line of lt_key_fields,
                   <key_cond>  type string,
                   <line>      type any,
                   <field>     type any.

    "// Create work area for the line
    create data ld_line type (table_name).
    assign ld_line->* to <line>.

    "// Set key fields on the line
    call function 'CHANGEDOCU_KEY_CHAR2ANY'
      exporting
        iv_tablekey      = key
        iv_tabname       = table_name
      importing
        es_struct_out    = <line>
      exceptions
        tabname_is_empty = 1
        nametab_error    = 2
        others           = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              into lv_msg.
      raise exception type zcx_planned_change_error
        exporting
          textid = zcx_planned_change_error=>error_message
          msg    = lv_msg.
    endif.

    "// Build key/WHERE conditions
    select fieldname from dd03l into table lt_key_fields
      where
        tabname = table_name and
        as4local = 'A' and
        keyflag = 'X'.
    if sy-subrc <> 0.
      raise exception type zcx_planned_change_error
        exporting
          textid     = zcx_planned_change_error=>invalid_parameter
          param_name = 'TABLE_NAME'.
    endif.

    loop at lt_key_fields assigning <key_field>.
      condense <key_field>.
      assign component <key_field> of structure <line> to <field>.
      if sy-subrc <> 0.
        raise exception type zcx_planned_change_error
          exporting
            textid     = zcx_planned_change_error=>invalid_parameter
            param_name = 'DD03L-FIELDNAME'.
      endif.
      append initial line to lt_key_conds assigning <key_cond>.
      concatenate <key_field> ' = ''' <field> '''' into <key_cond> respecting blanks.
    endloop.
    concatenate lines of lt_key_conds into lv_where_cond separated by ' AND '.


    "// Line was deleted?
    read table me->ins_cdpos transporting no fields
      with key tabname = table_name
               tabkey = key
               chngind = action_delete.
    if sy-subrc = 0.
      delete from (table_name) where (lv_where_cond).
      return.
    endif.

    "// Line was updated?
    read table me->ins_cdpos transporting no fields
      with key tabname = table_name
               tabkey = key
               chngind = action_update.
    if sy-subrc = 0.
      "// Find original line in the database
      select single * from (table_name) into <line> where (lv_where_cond).
      if sy-subrc <> 0.
        raise exception type zcx_planned_change_error
          exporting
            textid  = zcx_planned_change_error=>original_line_not_found
            tabname = table_name.
      endif.
      lv_operation = action_update.
    endif.

    "// Fill secondary fields
    loop at me->ins_cdpos assigning <cdpos>
      where tabname = table_name and
            tabkey = key.

      assign component <cdpos>-fname of structure <line> to <field>.
      if sy-subrc <> 0.
        raise exception type zcx_planned_change_error
          exporting
            textid     = zcx_planned_change_error=>invalid_parameter
            param_name = 'INS_CDPOS-FNAME'.
      endif.

      case <cdpos>-chngind.
        when action_delete_single_field. "// Delete (single field documetation)
          clear <field>.
        when others.
          <field> = <cdpos>-value_new.
      endcase.
    endloop.

    "// Update database table
    if lv_operation = action_update.
      update (table_name) from <line>.
    else.
      insert (table_name) from <line>.
    endif.
  endmethod.


  method clear_global_data.
    data: lt_globals   type table of rfieldlist,
          lv_fieldname type string.

    field-symbols: <global> like line of lt_globals,
                   <field>  type any,
                   <table>  type table.

    call function 'GET_GLOBAL_SYMBOLS'
      exporting
        program   = repid
      tables
        fieldlist = lt_globals.

    "// Clear every variable
    loop at lt_globals assigning <global>
      where
        name(1) na '<%' and "// Ignore field-symbols
        name <> 'SY' and
        name <> 'SYST' and
        flitl is initial. "// Ignore read-only
      "// Access variable via field-symbol -> (PROGRAM)SYMBOL
      concatenate '(' repid ')' <global>-name into lv_fieldname.
      case <global>-type.
        when cl_abap_typedescr=>typekind_table. "// Internal table
          assign (lv_fieldname) to <table>.
          if sy-subrc = 0.
            free <table>.
          endif.
        when others. "// Anything else
          assign (lv_fieldname) to <field>.
          if sy-subrc = 0.
            free <field>.
          endif.
      endcase.
    endloop.
  endmethod.


  method constructor.
    "// Initialize func.grp. SCD0
    clear_global_data( 'SAPLSCD0' ).

    call function 'CHANGEDOCUMENT_OPEN'
      exporting
        objectclass             = objectclas
        objectid                = objectid
        planned_or_real_changes = 'P'.
    sync_scd0_to_me( ).

    me->gs_cdhdr-objectclas = objectclas.
    me->gs_cdhdr-objectid = objectid.
    me->gs_cdhdr-changenr = changenr.
    me->gs_cdhdr-tcode = tcode.
    me->gs_cdhdr-udate = udate.
    me->gs_cdhdr-utime = utime.

    "// Loading existing Change Document?
    if me->gs_cdhdr-changenr is not initial.
      "// Read header
      select single * from pcdhdr
        into me->gs_cdhdr
        where
          objectclas = objectclas and
          objectid = objectid and
          changenr = changenr.
      if sy-subrc <> 0.
        raise exception type zcx_planned_change_error
          exporting
            textid     = zcx_planned_change_error=>not_found
            objectclas = objectclas
            objectid   = objectid
            changenr   = changenr.
      endif.

      "// Read changes
      select * from pcdpos
        into table me->ins_cdpos
        where
          objectclas = objectclas and
          objectid = objectid and
          changenr = changenr.

      "// Send back read data to func.grp.
      sync_me_to_scd0( ).
    endif.
  endmethod.


  method create.
    data: lv_objectid type cdobjectv.

    lv_objectid = objectid.
    create object instance
      exporting
        objectclas = objectclas
        objectid   = lv_objectid
        tcode      = tcode
        udate      = udate
        utime      = utime.
  endmethod.


  method delete.
    data: lv_msg type string.

    call function 'PLANNED_CHANGES_DELETE'
      exporting
        changenumber               = me->gs_cdhdr-changenr
        objectclass                = me->gs_cdhdr-objectclas
        objectid                   = me->gs_cdhdr-objectid
        planned_change_number_from = space
      exceptions
        no_changes_found           = 1
        others                     = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              into lv_msg.
      raise exception type zcx_planned_change_error
        exporting
          textid = zcx_planned_change_error=>error_message
          msg    = lv_msg.
    endif.
  endmethod.


  method get_table.
    data: lt_keys type ty_t_keys,
          ld_line type ref to data.

    field-symbols: <cdpos> like line of me->ins_cdpos,
                   <key>   like line of lt_keys,
                   <line>  type any.

    refresh table.

    "// Create work area for the table
    create data ld_line like line of table.
    assign ld_line->* to <line>.

    "// Build created/updated lines
    lt_keys = get_table_keys( table_name ).
    loop at lt_keys assigning <key>.
      get_table_line(
        exporting table_name = table_name
                  key = <key>
        changing line = <line>
      ).
      if <line> is not initial.
        append <line> to table.
      endif.
    endloop.

    free ld_line.
  endmethod.


  method get_table_keys.
    field-symbols: <cdpos> like line of me->ins_cdpos,
                   <key>   like line of keys.

    "// Group line keys
    loop at me->ins_cdpos assigning <cdpos>
      where tabname = table_name.
      append <cdpos>-tabkey to keys.
    endloop.
    sort keys. delete adjacent duplicates from keys.
  endmethod.


  method get_table_line.
    data: lt_key_fields type table of fieldname,
          lt_key_conds  type table of string,
          lv_where_cond type string,
          lv_msg        type string.

    field-symbols: <key_field> like line of lt_key_fields,
                   <key_cond>  type string,
                   <cdpos>     like line of me->ins_cdpos,
                   <field>     type any.

    clear line.

    "// Line was deleted?
    read table me->ins_cdpos transporting no fields
      with key tabname = table_name
               tabkey = key
               chngind = action_delete.
    if sy-subrc = 0.
      clear line.
      return.
    endif.

    "// Send key fields to line
    call function 'CHANGEDOCU_KEY_CHAR2ANY'
      exporting
        iv_tablekey      = key
        iv_tabname       = table_name
      importing
        es_struct_out    = line
      exceptions
        tabname_is_empty = 1
        nametab_error    = 2
        others           = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              into lv_msg.
      raise exception type zcx_planned_change_error
        exporting
          textid = zcx_planned_change_error=>error_message
          msg    = lv_msg.
    endif.

    "// Line was updated?
    read table me->ins_cdpos transporting no fields
      with key tabname = table_name
               tabkey = key
               chngind = action_update.
    if sy-subrc = 0.
      "// Find original line in the database
      select fieldname from dd03l into table lt_key_fields
        where
          tabname = table_name and
          as4local = 'A' and
          keyflag = 'X'.
      if sy-subrc <> 0.
        raise exception type zcx_planned_change_error
          exporting
            textid     = zcx_planned_change_error=>invalid_parameter
            param_name = 'TABLE_NAME'.
      endif.

      loop at lt_key_fields assigning <key_field>.
        condense <key_field>.
        assign component <key_field> of structure line to <field>.
        if sy-subrc <> 0.
          raise exception type zcx_planned_change_error
            exporting
              textid     = zcx_planned_change_error=>invalid_parameter
              param_name = 'DD03L-FIELDNAME'.
        endif.
        append initial line to lt_key_conds assigning <key_cond>.
        concatenate <key_field> ' = ''' <field> '''' into <key_cond> respecting blanks.
      endloop.
      concatenate lines of lt_key_conds into lv_where_cond separated by ' AND '.

      select single * from (table_name) into line where (lv_where_cond).
      if sy-subrc <> 0.
        raise exception type zcx_planned_change_error
          exporting
            textid  = zcx_planned_change_error=>original_line_not_found
            tabname = table_name.
      endif.
    endif.

    "// Build line (as it will be)
    loop at me->ins_cdpos assigning <cdpos>
      where tabname = table_name and
            tabkey = key.

      assign component <cdpos>-fname of structure line to <field>.
      if sy-subrc <> 0.
        raise exception type zcx_planned_change_error
          exporting
            textid     = zcx_planned_change_error=>invalid_parameter
            param_name = 'INS_CDPOS-FNAME'.
      endif.

      case <cdpos>-chngind.
        when action_delete_single_field. "// Delete (single field documetation)
          clear <field>.

        when others.
          <field> = <cdpos>-value_new.
      endcase.
    endloop.
  endmethod.


  method load.
    create object instance
      exporting
        objectclas = objectclas
        objectid   = objectid
        changenr   = changenr.
  endmethod.


  method save.
    data: lv_msg type string.

    call function 'CHANGEDOCUMENT_CLOSE'
      exporting
        date_of_change          = me->gs_cdhdr-udate
        objectclass             = me->gs_cdhdr-objectclas
        objectid                = me->gs_cdhdr-objectid
        tcode                   = me->gs_cdhdr-tcode
        time_of_change          = me->gs_cdhdr-utime
        username                = sy-uname
        planned_or_real_changes = 'P'
        no_change_pointers      = 'X'
      importing
        changenumber            = changenr
      exceptions
        header_insert_failed    = 1
        no_position_inserted    = 2
        object_invalid          = 3
        open_missing            = 4
        position_insert_failed  = 5
        others                  = 6.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              into lv_msg.
      raise exception type zcx_planned_change_error
        exporting
          textid = zcx_planned_change_error=>save_failed
          msg    = lv_msg.
    endif.
  endmethod.


  method sync_me_to_scd0.
    data: lv_fieldname type string.

    field-symbols: <struct_from> type any,
                   <struct_to>   type any,
                   <table_from>  type table,
                   <table_to>    type table.

    define sync_struct.
      concatenate 'me->' &1 into lv_fieldname.
      assign (lv_fieldname) to <struct_from>.
      concatenate '(SAPLSCD0)' &1 into lv_fieldname.
      assign (lv_fieldname) to <struct_to>.
      <struct_to> = <struct_from>.
    end-of-definition.

    define sync_table.
      concatenate 'me->' &1 '[]' into lv_fieldname.
      assign (lv_fieldname) to <table_from>.
      concatenate '(SAPLSCD0)' &1 '[]' into lv_fieldname.
      assign (lv_fieldname) to <table_to>.
      <table_to> = <table_from>.
    end-of-definition.

    sync_struct 'gs_cdhdr'.
    sync_table 'ins_cdpos'.
    sync_table 'gt_cdpos_uid'.
    sync_table 'gt_cdpos_str'.
  endmethod.


  method sync_scd0_to_me.
    data: lv_fieldname type string.

    field-symbols: <struct_from> type any,
                   <struct_to>   type any,
                   <table_from>  type table,
                   <table_to>    type table.

    define sync_struct.
      concatenate '(SAPLSCD0)' &1 into lv_fieldname.
      assign (lv_fieldname) to <struct_from>.
      concatenate 'me->' &1 into lv_fieldname.
      assign (lv_fieldname) to <struct_to>.
      <struct_to> = <struct_from>.
    end-of-definition.

    define sync_table.
      concatenate '(SAPLSCD0)' &1 '[]' into lv_fieldname.
      assign (lv_fieldname) to <table_from>.
      concatenate 'me->' &1 '[]' into lv_fieldname.
      assign (lv_fieldname) to <table_to>.
      <table_to> = <table_from>.
    end-of-definition.

    sync_struct 'gs_cdhdr'.
    sync_table 'ins_cdpos'.
    sync_table 'gt_cdpos_uid'.
    sync_table 'gt_cdpos_str'.
  endmethod.
ENDCLASS.
