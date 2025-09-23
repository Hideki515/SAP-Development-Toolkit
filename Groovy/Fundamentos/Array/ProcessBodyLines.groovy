import com.sap.gateway.ip.core.customdev.util.Message
import java.util.HashMap

/**
 * Exemplo: processar cada linha do Body no CPI
 * - Converte para maiúsculas
 * - Adiciona o sufixo "(PROCESSADO)"
 */
def Message processData(Message message) {

    // --- 1. Ler o Body ---
    def body = message.getBody(String)

    // Evitar null pointer caso body esteja vazio
    if (!body) {
        message.setBody("[Body estava vazio]")
        return message
    }

    // --- 2. Quebrar em linhas ---
    def lines = body.split("\n")

    // --- 3. Processar cada linha ---
    def processedLines = lines.collect { line ->
        line.toUpperCase() + " (PROCESSADO)"
    }

    // --- 4. Montar novo Body ---
    def newBody = processedLines.join("\n")

    // --- 5. Atualizar Body da mensagem ---
    message.setBody(newBody)

    return message
}
