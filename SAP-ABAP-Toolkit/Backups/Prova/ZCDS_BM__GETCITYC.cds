@AbapCatalog.sqlViewName: 'ZCDS_BM_CITYC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Estágio'

@ClientHandling.type: #CLIENT_DEPENDENT
@OData.publish: true

define view ZCDS_BM__GETCITYC
  with parameters
    p_regio : regio,
    p_bezei : bezei20
  as select from t005h
{
  key cityc

}
where
      spras = 'E'
  and land1 = 'BR'
  and regio = :p_regio
  and bezei = :p_bezei;
