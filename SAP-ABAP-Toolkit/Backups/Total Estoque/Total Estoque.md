Fazer uma tela de seleção com o campo WHNR onde o usuário poderá buscar pelo código do armazém. 

No primeiro ALV deve ser exibido o código do armazém (WHNR-MAT\_STOCK), a descrição do armazém ( DESCRIPTION-STOREHOUSE), a região do armazém (REGIO-STOREHOUSE) e soma da quantidade de todos os materiais no estoque (somar QUANTITY-MAT\_STOCK). 

O primeiro ALV deve ser exibido da seguinte maneira: 

![](Aspose.Words.d5d11067-5f4d-4071-8e77-41f1da45949c.001.png)

Deve ser incluído no FIELDCAT do ALV um campo MARK para que seja possível selecionar [^1][a](#_page0_x82.00_y758.92) linha que desejar ver mais detalhes. 

Para pegar qual linha foi selecionada, utilizar a função:  

`  `CALL FUNCTION 'GET\_GLOBALS\_FROM\_SLVC\_FULLSCR'     IMPORTING 

`      `e\_grid = e\_grid. 

E utilizar os métodos: 

`  `CALL METHOD e\_grid->check\_changed\_data. 

`  `CALL METHOD e\_grid->get\_selected\_rows     IMPORTING 

`      `et\_index\_rows = t\_selected\_rows. 

Com o retorno do método t\_selected\_rows, fazer um loop e read table com o index t\_selected\_rows e montar um range com os WHNR selecionados. 

Montar o segund o ALV trazendo os campos MOVNR (MOVNR-MAT\_HANDLING), WHNR (WHNR-MAT\_HANDLING), BRANR (BRANR-STOREHOUSE), DESCRIPTION (DESCRIPTION- STOREHOUSE), DOCTYP (DOCTYP-MAT\_HANDLING), MOVTYP (MOVTYP- MAT\_HANDLING), MATNR (MATNR-MATERIAL), MAKTX (MAKTX-MATERIAL), QUANTITY (QUANTITY-MAT\_HANDLING), PRICE (PRICE-MATERIAL), VALOR (QUANTITY- MAT\_HANDLING \* PRICE-MATERIAL), ERDAT (ERDAT-MAT\_HANDLING), ENTRYTIME (ENTRYTIME-MAT\_HANDLING). 

Exibir um segundo ALV com essas informações. 

[^1]: <a name="_page0_x82.00_y758.92"></a> Palavras na cor LARANJA são as tabelas que devem ser utilizadas.