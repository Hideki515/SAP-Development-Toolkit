import com.sap.gateway.ip.core.customdev.util.Message
import groovy.xml.MarkupBuilder

def Message processData(Message message) {
    
    // 1. Lista de códigos que precisam ser processados
    def codesToProcess = ["A10", "B25", "C33", "D40", "E55"]
    
    // 2. Variável que vai acumular o resultado final (em formato de texto)
    def resultBody = "Códigos Processados:\n"
    
    // 3. Estrutura for...in: percorre cada elemento da lista
    for (code in codesToProcess) {
        // 4. Simula o processamento de cada código
        //    Aqui você pode trocar a lógica por algo real (ex.: validação, cálculo etc.)
        resultBody += "- Código: " + code + " -> OK\n"
    }
    
    // 5. Define o corpo da mensagem como o texto gerado
    message.setBody(resultBody)
    
    // 6. Retorna a mensagem para continuar o fluxo
    return message
}
