import com.sap.gateway.ip.core.customdev.util.Message   // Importa a classe Message usada em scripts do SAP CPI
import groovy.xml.MarkupBuilder                         // Importa a classe para construir XML (não é usada nesse exemplo)

// Função Principal (ponto de entrada obrigatório no Groovy Script do CPI)
def Message processData(Message message) {
    
    def currentBody = message.getBody(java.lang.String);   // Obtém o corpo da mensagem como String
    
    // 1. Chama a função auxiliar para limpar o corpo da mensagem
    def cleanBody = sanitizeData(currentBody);
    
    // 2. Define o corpo da mensagem com o dado já limpo
    message.setBody(cleanBody);
    
    return message;   // Retorna a mensagem modificada
}

// Função Auxiliar: responsável por limpar/sanitizar o conteúdo recebido
def sanitizeData(String rawData) {
    if (rawData == null) {           // Se o corpo for nulo (não existir), retorna vazio
        return "";
    }
    
    // 1. Remove espaços em branco no início e no fim da String
    def cleanedData = rawData.trim();
    
    // 2. Substitui múltiplas quebras de linha (\r ou \n) por apenas uma
    cleanedData = cleanedData.replaceAll(/[\r\n]+/, "\n");
    
    // 3. Remove caracteres especiais que não sejam:
    //    - letras, números (\w)
    //    - espaços (\s)
    //    - quebras de linha (\n)
    //    - hífen (-)
    cleanedData = cleanedData.replaceAll(/[^\w\s\n-]/, "");
    
    return cleanedData;   // Retorna o corpo já limpo
}
