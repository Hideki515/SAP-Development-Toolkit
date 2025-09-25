import com.sap.gateway.ip.core.customdev.util.Message
import java.util.HashMap

def Message processData(Message message) {
    
    // 1. Obtém o Map contendo todos os Headers da mensagem
    def headersMap = message.getHeaders();
    
    // 2. Cria uma variável string para armazenar a listagem formatada dos headers
    def headerLog = "Headers Encontrados:\n";

    // 3. Percorre cada par chave/valor do Map de headers
    headersMap.each { key, value ->
        // 4. Concatena o nome do header e seu valor à string (com identação e quebra de linha)
        headerLog += "  - " + key + ": " + value + "\n";
    }

    // 5. Recupera o corpo original da mensagem em formato String
    def originalBody = message.getBody(java.lang.String);

    // 6. Junta a listagem de headers com o corpo original, separando por um marcador
    def finalBody = headerLog + "\n--- Corpo Original ---\n" + originalBody;

    // 7. Substitui o corpo da mensagem pelo novo conteúdo combinado
    message.setBody(finalBody);

    // 8. Retorna a mensagem processada para o fluxo continuar
    return message;
}
