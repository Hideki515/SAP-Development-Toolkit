import com.sap.gateway.ip.core.customdev.util.Message
import java.util.HashMap

/**
 * Exemplo de estudo: lendo uma Property do CPI e criando outra
 */
def Message processData(Message message) {

    // --- 1. Ler uma Property existente ---
    // Aqui buscamos no contexto a property "nomeUsuario".
    // Se não existir, será retornado null.
    def nomeUsuario = message.getProperty("nomeUsuario")

    // --- 2. Validar se a property veio preenchida ---
    if (!nomeUsuario) {
        // Se estiver vazia ou null, podemos definir um valor padrão.
        nomeUsuario = "[Nome não informado]"
    }

    // --- 3. Gravar uma nova Property ---
    // Criamos/atualizamos a property "Nome" com o valor obtido.
    message.setProperty("Nome", nomeUsuario)

    // --- 4. Retornar a mensagem ---
    return message
}