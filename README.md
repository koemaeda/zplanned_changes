# ZPlanned_Change

Class for Planned Changes creation and manipulation in ABAP.

Planned changes are a type of [Change Documents](https://help.sap.com/saphelp_nw70/helpdata/en/2a/fa015b493111d182b70000e829fbfe/frameset.htm) that only describes changes to database tables, but that are not yet actual.

Using this functionality, it's possible to plan ahead and manipulate complex database changes (such as planned Materials and Vendors), and schedule the activation of these changes for a future date.

These changes are stored in tables `PCDHDR` and `PCDPOS`.

## Installation

To install, you need [abapGit](https://github.com/larshp/abapGit).

The unit tests can be run on class ZPLANNED_CHANGE via ABAPUnit.

## Example code

```language-abap
  data: s_old type scarr,
        s_new type scarr.

  select single * from scarr into s_old
    where carrid = 'AA'.

  s_new = s_old.
  s_new-url = 'http://www.aa.com/new-site'.

  try.
    "// Create a new planned change (to be applied next week)
      data(change) = zplanned_change=>create(
        objectclas = '$EXAMPLE'
        objectid = '12345'
        tcode = 'SE38'
        udate = conv #( sy-datum + 7 )
        utime = '120000'
      ).

    "// Add our updated table line to the change
    change->add_table_line(
      old = s_old
      new = s_new
      change_indicator = zplanned_change=>action_update
    ).

    "// Save the Change Document
    change->save( ).
    commit work.

  catch zcx_planned_change_error into data(exc).
    message exc->get_text( ) type 'E'.
  endtry.
  ```
