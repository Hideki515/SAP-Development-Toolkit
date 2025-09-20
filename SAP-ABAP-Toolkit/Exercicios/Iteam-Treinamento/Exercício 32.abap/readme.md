## Exercício: Cadastro de Bebidas via Batch Input no SAP

**Objetivo:** Desenvolver uma Call Transaction (programa ABAP) para automatizar o cadastro de materiais do tipo "Bebidas" no SAP, utilizando a transação `MM01` e a técnica de Batch Input.

**Requisitos:**

1. **Criar um programa ABAP:** O programa deverá ler os dados de um arquivo TXT e realizar o cadastro dos materiais na transação `MM01` utilizando Batch Input.
2. **Criar um arquivo TXT:** O arquivo TXT conterá os dados dos materiais a serem cadastrados, seguindo o formato delimitado por TAB e a estrutura descrita na seção "Estrutura do Arquivo TXT" abaixo.
3. **Tratamento de erros:** O programa deverá ser capaz de identificar e tratar erros durante a execução do Batch Input, como por exemplo, a tentativa de cadastro de um material já existente. Um log detalhado dos erros deve ser apresentado ao usuário.
4. **Log de sucesso:** O programa deverá gerar um log com os materiais cadastrados com sucesso, informando o código do material e uma mensagem de sucesso. 

**Estrutura do Arquivo TXT:**

* Consulte o arquivo `estrutura_arquivo_bebidas.md` para obter detalhes sobre a estrutura do arquivo TXT.

**Exemplo de arquivo:**

* Consulte o arquivo `exemplo_arquivo_bebidas.txt` para visualizar um exemplo de arquivo TXT preenchido com dados de exemplo.

**Observações:**

* Este exercício exige conhecimento de programação ABAP e da técnica de Batch Input.
* A estrutura do arquivo TXT e o programa ABAP devem ser desenvolvidos considerando as necessidades específicas do cenário, como campos obrigatórios, validações e personalizações.

**Dica:**

* Utilize a transação `SHDB` (Gravação de Batch Input) para registrar os passos manuais de cadastro de um material na transação `MM01`. Essa gravação pode ser utilizada como base para o desenvolvimento do programa ABAP. 

**Avaliação:**

* O exercício será avaliado com base na funcionalidade do programa ABAP, na estrutura do arquivo TXT, no tratamento de erros, na geração do log e na qualidade do código.
