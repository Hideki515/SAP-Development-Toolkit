import com.sap.gateway.ip.core.customdev.util.Message
import java.util.HashMap

/**
 * Exemplo de estudo: como ler, modificar e sobrescrever o Body no SAP CPI usando Groovy
 */
def Message processData(Message message) {

    // --- 1. Ler o Body atual da mensagem ---
    // Aqui usamos getBody(String) para garantir que o conteúdo será tratado como texto (String).
    def body = message.getBody(String)

    // --- 2. Verificar se o Body está vazio ou nulo ---
    // Isso evita erros caso o body venha null.
    if (!body) {
        body = "[Body estava vazio]"
    }

    // --- 3. Modificar o conteúdo do Body ---
    // Concatenamos uma string fixa antes do conteúdo original.
    def novoBody = "Conteúdo processado no Groovy:\n${body}"

    // --- 4. Atualizar o Body da mensagem ---
    // Aqui sobrescrevemos o body com o novo conteúdo.
    message.setBody(novoBody as String)

    // --- 5. Retornar a mensagem para o fluxo continuar ---
    return message
}