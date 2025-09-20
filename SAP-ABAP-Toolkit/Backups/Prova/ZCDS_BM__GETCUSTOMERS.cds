@AbapCatalog.sqlViewName: 'ZCDS_BM__GETCUST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Estágio'

@ClientHandling.type: #CLIENT_DEPENDENT
@OData.publish: true

define view ZCDS_BM__GETCUSTOMERS
  with parameters
    p_custnr : custnr
  as select from customer as Customer
  association [0..1] to address as Address on $projection.addrnr = Address.addrnr
{
  key Customer.custnr,
      Customer.name,
      Customer.name2,
      Customer.cnpj,
      Customer.cpf,
      Customer.addrnr,
      Address.title,
      Address.name  as nameAdress,
      Address.name2 as nameAdress2,
      Address.city,
      Address.post_code,
      Address.neighborhood,
      Address.street,
      Address.complement,
      Address.house_num,
      Address.building,
      Address.floor,
      Address.roomnumber
}
where
  Customer.custnr = :p_custnr;
