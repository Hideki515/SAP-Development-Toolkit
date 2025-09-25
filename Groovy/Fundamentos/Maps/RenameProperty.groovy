import com.sap.gateway.ip.core.customdev.util.Message
import java.util.HashMap

def Message processData(Message message) {
    // 1. Obtém o Map de Propriedades da Mensagem
    def propertiesMap = message.getProperties();

    // 2. Define a chave antiga e a nova chave
    // Exemplo: renomear "ID_LEGADO" para "Customer_ID"
    def oldKey = "ID_LEGADO";
    def newKey = "Customer_ID";

    // 3. Verifica se a propriedade antiga existe
    if (propertiesMap.containsKey(oldKey)) {
        
        // 4. Obtém o valor da propriedade antiga
        def customerIdValue = propertiesMap.get(oldKey);

        // 5. Cria a nova chave com o valor antigo
        propertiesMap.put(newKey, customerIdValue);
        
        // 6. Remove a chave antiga para evitar duplicidade
        propertiesMap.remove(oldKey);

        // 7. Log informativo (apenas para debug)
        def messageLog = messageLogFactory.getMessageLog(message);
        if (messageLog != null) {
            messageLog.addAttachmentAsString(
                "Renomear Property",
                "Propriedade renomeada: ${oldKey} -> ${newKey} com valor: ${customerIdValue}",
                "text/plain"
            );
        }

    } else {
        // Caso a chave antiga não exista
        def messageLog = messageLogFactory.getMessageLog(message);
        if (messageLog != null) {
            messageLog.addAttachmentAsString(
                "Renomear Property - Aviso",
                "Chave antiga (${oldKey}) não encontrada nas propriedades.",
                "text/plain"
            );
        }
    }

    // Retorna a mensagem com o Map de propriedades atualizado
    return message;
}
