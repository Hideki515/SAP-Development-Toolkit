@AbapCatalog.sqlViewName: 'ZVJOIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS com Join'
@Metadata.ignorePropagatedAnnotations: true

define view ZCDS_BHS_JOINS
  as select from    spfli
    inner join scarr on spfli.carrid = scarr.carrid
{
  key spfli.carrid    as Carrid,
  key spfli.connid    as Connid,
      spfli.countryfr as Countryfr,
      spfli.cityfrom  as Cityfrom,
      spfli.airpfrom  as Airpfrom,
      spfli.countryto as Countryto,
      spfli.cityto    as Cityto,
      spfli.airpto    as Airpto,
      spfli.fltime    as Fltime,
      spfli.deptime   as Deptime,
      spfli.arrtime   as Arrtime,
      spfli.distance  as Distance,
      spfli.distid    as Distid,
      spfli.fltype    as Fltype,
      spfli.period    as Period
}
