class abap_unit_testclass definition deferred.
class zplanned_change definition local friends abap_unit_testclass.

*----------------------------------------------------------------------*
*       CLASS abap_unit_testclass DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class abap_unit_testclass definition for testing.
  "#AU Duration Short
  "#AU Risk_Level Harmless

  private section.
    data:
      m_objectid type cdobjectv,
      m_ref      type ref to zplanned_change.               "#EC NOTEXT

    class-methods: class_setup.

    methods: setup.
    methods: teardown.

    methods:
      line_creation for testing raising zcx_planned_change_error,
      line_deletion for testing raising zcx_planned_change_error,
      single_field_update for testing raising zcx_planned_change_error,
      multiple_field_update for testing  raising zcx_planned_change_error,
      table_creation for testing raising zcx_planned_change_error,
      table_update for testing raising zcx_planned_change_error,
      table_deletion for testing raising zcx_planned_change_error,
      table_mixed for testing raising zcx_planned_change_error.
endclass.       "Abap_Unit_Testclass

*----------------------------------------------------------------------*
*       CLASS abap_unit_testclass IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class abap_unit_testclass implementation.
  method class_setup.
    call function 'RANDOM_INITIALIZE'.
  endmethod.                    "class_setup

  method setup.
    data: lv_value type datatype-char0128.
    call function 'RANDOM_C'
      exporting
        len_min   = 90
        len_max   = 90
        char_min  = 1
        char_max  = 26
      importing
        rnd_value = lv_value.
    m_objectid = lv_value.
  endmethod.       "Setup

  method teardown.
    free m_ref.
  endmethod.       "Teardown


  method line_creation.
    data: ls_scarr        type scarr,
          ls_scarr_result type scarr,
          lv_changenr     type cdchangenr,
          lt_keys         type zplanned_change=>ty_t_keys,
          lo_exc          type ref to zcx_planned_change_error.

    field-symbols: <key> like line of lt_keys.

    "// Setup
    delete from scarr where carrid = '123'.
    commit work and wait.

    ls_scarr-mandt = sy-mandt.
    ls_scarr-carrid = '123'.
    ls_scarr-carrname = 'One Two Three'.
    ls_scarr-currcode = 'BRL'.
    ls_scarr-url = 'goatse.cx'.

    m_ref = zplanned_change=>create(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->add_table_line( new = ls_scarr ).

    lv_changenr = m_ref->save( ).
    cl_aunit_assert=>assert_not_initial( lv_changenr ).
    free m_ref.

    "// Check if the Change Document was saved
    select count( * ) from pcdpos into sy-dbcnt
      where
        objectclas = '$ABAPUnit' and
        objectid = m_objectid and
        changenr = lv_changenr.
    cl_aunit_assert=>assert_equals(
      exp = 3
      act = sy-dbcnt
    ).

    "// Load saved change
    m_ref = zplanned_change=>load(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
      changenr = lv_changenr
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->apply( ).
    commit work and wait.

    "// Check if the line was created
    lt_keys = m_ref->get_table_keys( 'SCARR' ).
    cl_aunit_assert=>assert_not_initial( lt_keys ).
    read table lt_keys assigning <key> index 1.
    m_ref->get_table_line(
      exporting table_name = 'SCARR'
                key = <key>
      changing line = ls_scarr_result
    ).
    cl_aunit_assert=>assert_equals(
      exp = ls_scarr
      act = ls_scarr_result
    ).
    clear ls_scarr_result.
    select single * from scarr into ls_scarr_result
      where carrid = '123'.
    cl_aunit_assert=>assert_subrc( sy-subrc ).
    cl_aunit_assert=>assert_equals(
      exp = ls_scarr
      act = ls_scarr_result
    ).

    "// Check if the Change Document was deleted
    free m_ref.
    try.
        m_ref = zplanned_change=>load(
          objectclas = '$ABAPUnit'
          objectid = m_objectid
          changenr = lv_changenr
        ).
      catch zcx_planned_change_error into lo_exc.
        cl_aunit_assert=>assert_equals(
          exp = zcx_planned_change_error=>not_found
          act = lo_exc->textid
        ).
    endtry.
  endmethod.                    "line_creation


  method line_deletion.
    data: ls_scarr        type scarr,
          ls_scarr_result type scarr,
          lv_changenr     type cdchangenr,
          lo_exc          type ref to zcx_planned_change_error.

    ls_scarr-mandt = sy-mandt.
    ls_scarr-carrid = '123'.

    m_ref = zplanned_change=>create(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->add_table_line(
      change_indicator = zplanned_change=>action_delete "// delete
      old = ls_scarr
    ).

    lv_changenr = m_ref->save( ).
    cl_aunit_assert=>assert_not_initial( lv_changenr ).
    free m_ref.

    "// Check if the Change Document was saved
    select count( * ) from pcdpos into sy-dbcnt
      where
        objectclas = '$ABAPUnit' and
        objectid = m_objectid and
        changenr = lv_changenr.
    cl_aunit_assert=>assert_equals(
      exp = 1
      act = sy-dbcnt
    ).

    "// Load saved document
    m_ref = zplanned_change=>load(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
      changenr = lv_changenr
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->apply( ).
    commit work and wait.

    "// Check if the line was deleted
    select single * from scarr into ls_scarr_result
      where carrid = '123'.
    cl_aunit_assert=>assert_subrc(
      exp = 4
      act = sy-subrc
    ).
  endmethod.                    "line_deletion


  method single_field_update.
    data: ls_scarr_old    type scarr,
          ls_scarr_new    type scarr,
          ls_scarr_result type scarr,
          lv_changenr     type cdchangenr,
          lo_exc          type ref to zcx_planned_change_error.

    "// Setup
    ls_scarr_old-mandt = sy-mandt.
    ls_scarr_old-carrid = '123'.
    ls_scarr_old-carrname = 'One Two Three'.
    ls_scarr_old-currcode = 'BRL'.
    ls_scarr_old-url = 'goatse.cx'.
    modify scarr from ls_scarr_old.
    commit work and wait.

    ls_scarr_new = ls_scarr_old.
    ls_scarr_new-url = 'bonsaikitten.com'.

    m_ref = zplanned_change=>create(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->add_table_line(
      change_indicator = zplanned_change=>action_update
      new = ls_scarr_new
      old = ls_scarr_old
    ).

    lv_changenr = m_ref->save( ).
    cl_aunit_assert=>assert_not_initial( lv_changenr ).
    free m_ref.

    "// Check if the Change Document was saved
    select count( * ) from pcdpos into sy-dbcnt
      where
        objectclas = '$ABAPUnit' and
        objectid = m_objectid and
        changenr = lv_changenr.
    cl_aunit_assert=>assert_equals(
      exp = 1
      act = sy-dbcnt
    ).

    "// Load saved document
    m_ref = zplanned_change=>load(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
      changenr = lv_changenr
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->apply( ).
    commit work and wait.

    "// Check if the line was updated
    select single * from scarr into ls_scarr_result
      where carrid = '123'.
    cl_aunit_assert=>assert_subrc( sy-subrc ).
    cl_aunit_assert=>assert_equals(
      exp = ls_scarr_new
      act = ls_scarr_result
    ).
  endmethod.                    "single_field_update


  method multiple_field_update.
    data: ls_scarr_old    type scarr,
          ls_scarr_new    type scarr,
          ls_scarr_result type scarr,
          lv_changenr     type cdchangenr,
          lo_exc          type ref to zcx_planned_change_error.

    "// Setup
    ls_scarr_old-mandt = sy-mandt.
    ls_scarr_old-carrid = '123'.
    ls_scarr_old-carrname = 'One Two Three'.
    ls_scarr_old-currcode = 'BRL'.
    ls_scarr_old-url = 'goatse.cx'.
    modify scarr from ls_scarr_old.
    commit work and wait.

    ls_scarr_new = ls_scarr_old.
    ls_scarr_new-currcode = 'USD'.
    ls_scarr_new-url = 'bonsaikitten.com'.

    m_ref = zplanned_change=>create(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->add_table_line(
      change_indicator = zplanned_change=>action_update
      new = ls_scarr_new
      old = ls_scarr_old
    ).

    lv_changenr = m_ref->save( ).
    cl_aunit_assert=>assert_not_initial( lv_changenr ).
    free m_ref.

    "// Check if the Change Document was saved
    select count( * ) from pcdpos into sy-dbcnt
      where
        objectclas = '$ABAPUnit' and
        objectid = m_objectid and
        changenr = lv_changenr.
    cl_aunit_assert=>assert_equals(
      exp = 2
      act = sy-dbcnt
    ).

    "// Load saved document
    m_ref = zplanned_change=>load(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
      changenr = lv_changenr
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->apply( ).
    commit work and wait.

    "// Check if the line was updated
    select single * from scarr into ls_scarr_result
      where carrid = '123'.
    cl_aunit_assert=>assert_subrc( sy-subrc ).
    cl_aunit_assert=>assert_equals(
      exp = ls_scarr_new
      act = ls_scarr_result
    ).
  endmethod.                    "multiple_field_update


  method table_creation.
    data: lt_spfli        type table of spfli,
          lt_spfli_result type table of spfli,
          lv_changenr     type cdchangenr,
          lo_exc          type ref to zcx_planned_change_error.

    field-symbols: <spfli> like line of lt_spfli.

    "// Setup
    delete from spfli where carrid = '123'.
    commit work and wait.

    append initial line to lt_spfli assigning <spfli>.
    <spfli>-mandt = sy-mandt.
    <spfli>-carrid = '123'.
    <spfli>-connid = 1.
    <spfli>-countryfr = 'BR'.
    <spfli>-cityfrom = 'São Paulo'.
    <spfli>-airpfrom = 'GRU'.
    <spfli>-countryto = 'CL'.
    <spfli>-cityto = 'Santiago'.
    <spfli>-airpto = 'SCL'.

    append initial line to lt_spfli assigning <spfli>.
    <spfli>-mandt = sy-mandt.
    <spfli>-carrid = '123'.
    <spfli>-connid = 2.
    <spfli>-countryfr = 'CL'.
    <spfli>-cityfrom = 'Santiago'.
    <spfli>-airpfrom = 'SCL'.
    <spfli>-countryto = 'BR'.
    <spfli>-cityto = 'São Paulo'.
    <spfli>-airpto = 'GRU'.

    m_ref = zplanned_change=>create(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->add_table( new = lt_spfli ).

    lv_changenr = m_ref->save( ).
    cl_aunit_assert=>assert_not_initial( lv_changenr ).
    free m_ref.

    "// Check if the Change Document was created
    select count( * ) from pcdpos into sy-dbcnt
      where
        objectclas = '$ABAPUnit' and
        objectid = m_objectid and
        changenr = lv_changenr.
    cl_aunit_assert=>assert_equals(
      exp = 12
      act = sy-dbcnt
    ).

    "// Load saved document
    m_ref = zplanned_change=>load(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
      changenr = lv_changenr
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->apply( ).
    commit work and wait.

    "// Check if the lines were created
    m_ref->get_table(
      exporting table_name = 'SPFLI'
      changing table = lt_spfli_result
    ).
    cl_aunit_assert=>assert_equals(
      exp = lt_spfli
      act = lt_spfli_result
    ).
    clear lt_spfli_result.
    select * from spfli into table lt_spfli_result
      where carrid = '123'.
    cl_aunit_assert=>assert_subrc( sy-subrc ).
    cl_aunit_assert=>assert_equals(
      exp = lt_spfli
      act = lt_spfli_result
    ).
  endmethod.                    "table_creation


  method table_update.
    data: lt_spfli_old    type table of spfli,
          lt_spfli_new    type table of spfli,
          lt_spfli_result type table of spfli,
          lv_changenr     type cdchangenr,
          lo_exc          type ref to zcx_planned_change_error.

    field-symbols: <spfli> like line of lt_spfli_new.

    "// Setup
    delete from spfli where carrid = '123'.
    append initial line to lt_spfli_old assigning <spfli>.
    <spfli>-mandt = sy-mandt.
    <spfli>-carrid = '123'.
    <spfli>-connid = 1.
    <spfli>-countryfr = 'BR'.
    <spfli>-cityfrom = 'São Paulo'.
    <spfli>-airpfrom = 'GRU'.
    <spfli>-countryto = 'CL'.
    <spfli>-cityto = 'Santiago'.
    <spfli>-airpto = 'SCL'.
    append initial line to lt_spfli_old assigning <spfli>.
    <spfli>-mandt = sy-mandt.
    <spfli>-carrid = '123'.
    <spfli>-connid = 2.
    <spfli>-countryfr = 'CL'.
    <spfli>-cityfrom = 'Santiago'.
    <spfli>-airpfrom = 'SCL'.
    <spfli>-countryto = 'BR'.
    <spfli>-cityto = 'São Paulo'.
    <spfli>-airpto = 'GRU'.
    modify spfli from table lt_spfli_old.
    commit work and wait.

    lt_spfli_new = lt_spfli_old.
    loop at lt_spfli_new assigning <spfli>.
      <spfli>-fltime = 360.
      <spfli>-distance = 4000.
    endloop.

    m_ref = zplanned_change=>create(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->add_table(
      change_indicator = zplanned_change=>action_update
      new = lt_spfli_new
      old = lt_spfli_old
    ).

    lv_changenr = m_ref->save( ).
    cl_aunit_assert=>assert_not_initial( lv_changenr ).
    free m_ref.

    "// Check if the Change Document was created
    select count( * ) from pcdpos into sy-dbcnt
      where
        objectclas = '$ABAPUnit' and
        objectid = m_objectid and
        changenr = lv_changenr.
    cl_aunit_assert=>assert_equals(
      exp = 4
      act = sy-dbcnt
    ).

    "// Load saved document
    m_ref = zplanned_change=>load(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
      changenr = lv_changenr
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->apply( ).
    commit work and wait.

    "// Check if the lines were updated
    select * from spfli into table lt_spfli_result
      where carrid = '123'.
    cl_aunit_assert=>assert_subrc( sy-subrc ).
    cl_aunit_assert=>assert_equals(
      exp = lt_spfli_new
      act = lt_spfli_result
    ).
  endmethod.                    "table_update


  method table_deletion.
    data: lt_spfli_old    type table of spfli,
          lt_spfli_result type table of spfli,
          lv_changenr     type cdchangenr,
          lo_exc          type ref to zcx_planned_change_error.

    field-symbols: <spfli> like line of lt_spfli_old.

    "// Setup
    delete from spfli where carrid = '123'.
    append initial line to lt_spfli_old assigning <spfli>.
    <spfli>-mandt = sy-mandt.
    <spfli>-carrid = '123'.
    <spfli>-connid = 1.
    <spfli>-countryfr = 'BR'.
    <spfli>-cityfrom = 'São Paulo'.
    <spfli>-airpfrom = 'GRU'.
    <spfli>-countryto = 'CL'.
    <spfli>-cityto = 'Santiago'.
    <spfli>-airpto = 'SCL'.
    append initial line to lt_spfli_old assigning <spfli>.
    <spfli>-mandt = sy-mandt.
    <spfli>-carrid = '123'.
    <spfli>-connid = 2.
    <spfli>-countryfr = 'CL'.
    <spfli>-cityfrom = 'Santiago'.
    <spfli>-airpfrom = 'SCL'.
    <spfli>-countryto = 'BR'.
    <spfli>-cityto = 'São Paulo'.
    <spfli>-airpto = 'GRU'.
    modify spfli from table lt_spfli_old.
    commit work and wait.

    m_ref = zplanned_change=>create(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->add_table(
      change_indicator = zplanned_change=>action_delete
      old = lt_spfli_old
    ).

    lv_changenr = m_ref->save( ).
    cl_aunit_assert=>assert_not_initial( lv_changenr ).
    free m_ref.

    "// Check if the Change Document was created
    select count( * ) from pcdpos into sy-dbcnt
      where
        objectclas = '$ABAPUnit' and
        objectid = m_objectid and
        changenr = lv_changenr.
    cl_aunit_assert=>assert_equals(
      exp = 2
      act = sy-dbcnt
    ).

    "// Load saved document
    m_ref = zplanned_change=>load(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
      changenr = lv_changenr
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->apply( ).
    commit work and wait.

    "// Check if the lines were deleted
    select * from spfli into table lt_spfli_result
      where carrid = '123'.
    cl_aunit_assert=>assert_subrc(
      exp = 4
      act = sy-subrc
    ).
  endmethod.                    "table_deletion


  method table_mixed.
    data: lt_spfli_new    type table of spfli,
          lt_spfli_old    type table of spfli,
          lt_spfli_result type table of spfli,
          lv_changenr     type cdchangenr,
          lo_exc          type ref to zcx_planned_change_error.

    field-symbols: <spfli> like line of lt_spfli_old.

    "// Setup
    delete from spfli where carrid = '123'.
    append initial line to lt_spfli_old assigning <spfli>.
    <spfli>-mandt = sy-mandt.
    <spfli>-carrid = '123'.
    <spfli>-connid = 1.
    <spfli>-countryfr = 'BR'.
    <spfli>-cityfrom = 'São Paulo'.
    <spfli>-airpfrom = 'GRU'.
    <spfli>-countryto = 'CL'.
    <spfli>-cityto = 'Santiago'.
    <spfli>-airpto = 'SCL'.
    append initial line to lt_spfli_old assigning <spfli>.
    <spfli>-mandt = sy-mandt.
    <spfli>-carrid = '123'.
    <spfli>-connid = 2.
    <spfli>-countryfr = 'CL'.
    <spfli>-cityfrom = 'Santiago'.
    <spfli>-airpfrom = 'SCL'.
    <spfli>-countryto = 'BR'.
    <spfli>-cityto = 'São Paulo'.
    <spfli>-airpto = 'GRU'.
    modify spfli from table lt_spfli_old.
    commit work and wait.

    lt_spfli_new = lt_spfli_old.

    read table lt_spfli_new index 1 assigning <spfli>.
    <spfli>-cityfrom = 'Campinas'.
    <spfli>-airpfrom = 'VCP'.

    delete lt_spfli_new index 2.

    append initial line to lt_spfli_new assigning <spfli>.
    <spfli>-mandt = sy-mandt.
    <spfli>-carrid = '123'.
    <spfli>-connid = 3.
    <spfli>-countryfr = 'AR'.
    <spfli>-cityfrom = 'Buenos Aires'.
    <spfli>-airpfrom = 'AEP'.
    <spfli>-countryto = 'BR'.
    <spfli>-cityto = 'Campinas'.
    <spfli>-airpto = 'VCP'.

    m_ref = zplanned_change=>create(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->add_table(
      change_indicator = zplanned_change=>action_update
      new = lt_spfli_new
      old = lt_spfli_old
    ).

    lv_changenr = m_ref->save( ).
    cl_aunit_assert=>assert_not_initial( lv_changenr ).
    free m_ref.

    "// Check if the Change Document was created
    select count( * ) from pcdpos into sy-dbcnt
      where
        objectclas = '$ABAPUnit' and
        objectid = m_objectid and
        changenr = lv_changenr.
    cl_aunit_assert=>assert_equals(
      exp = 9 "// 1 D + 2 U + 6 I
      act = sy-dbcnt
    ).

    "// Load saved document
    m_ref = zplanned_change=>load(
      objectclas = '$ABAPUnit'
      objectid = m_objectid
      changenr = lv_changenr
    ).
    cl_aunit_assert=>assert_not_initial( m_ref ).

    m_ref->apply( ).
    commit work and wait.

    "// Check if the table contents are correct
    select * from spfli into table lt_spfli_result
      where carrid = '123'.
    cl_aunit_assert=>assert_subrc( sy-subrc ).
    cl_aunit_assert=>assert_equals(
      exp = lt_spfli_new
      act = lt_spfli_result
    ).
  endmethod.                    "table_mixed

endclass.       "Abap_Unit_Testclass
