/* Estas linhas são apenas comentários de documentação, ignoradas durante a execução. Elas oferecem links para a documentação oficial da SAP sobre scripts e suas APIs. */

import com.sap.gateway.ip.core.customdev.util.Message; 
/* Esta linha importa a classe Message, que é a representação do objeto de mensagem principal no CPI. Ela contém o corpo, cabeçalhos e propriedades que você vai manipular. */

def Message processData(Message message) {

    message.setBody("Hello World!");
    /* Ela define o corpo da mensagem para a string "Hello World!", sobrescrevendo qualquer conteúdo anterior. */
    
    return message;
    /* Retorna o objeto 'message' modificado. Este será o input para o próximo passo no seu fluxo de integração do CPI. */
}