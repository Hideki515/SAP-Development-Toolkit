@AbapCatalog.sqlViewName: 'ZCDS_BM_MOV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Movimento Armazém'
define view ZCDS_BM_MOVIMENTO
  as select from mat_handling as Handling
    inner join   storehouse   as Store on Store.whnr = Handling.whnr
    inner join   material     as Mat   on Mat.matnr = Handling.matnr
    inner join   branch       as Bra   on Bra.branr = Handling.whnr
{
  key Handling.movnr,
      Bra.branr,
      Bra.compnr,
      Store.whnr,
      Store.description,
      Mat.matnr,
      Mat.maktx,
      Handling.doctype,
      Handling.movtyp,
      Handling.quantity,
      Mat.price,
      Handling.quantity * Mat.price as valor,
      Handling.erdat,
      Handling.entrytime
}
