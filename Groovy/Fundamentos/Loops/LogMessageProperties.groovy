import com.sap.gateway.ip.core.customdev.util.Message
import groovy.xml.MarkupBuilder                        

def Message processData(Message message) {
    
    // Obtém o Map de Propriedades da Mensagem (todas as propriedades definidas no fluxo até aqui)
    def propertiesMap = message.getProperties()
    
    // String que armazenará os logs formatados das propriedades
    def logMessage = "Detalhes das Propriedades:\n"
    
    // Adiciona algumas propriedades de exemplo no Map (útil apenas para testes locais)
    propertiesMap.put("ID_Cliente", "CUST123")
    propertiesMap.put("Status", "PENDENTE")

    // Loop each: percorre o Map de propriedades
    // Para cada par (chave, valor), executa o bloco de código
    propertiesMap.each { key, value ->
        // Concatena a chave e o valor atuais na string de log
        // Usa GString (interpolação de variáveis com ${})
        logMessage += "-> Chave: ${key}, Valor: ${value}\n"
    }
    
    // Substitui o corpo da mensagem pelo log montado acima
    message.setBody(logMessage)
    
    // Retorna o objeto Message atualizado para o CPI continuar o processamento
    return message
}
