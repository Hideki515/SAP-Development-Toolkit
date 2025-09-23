import com.sap.gateway.ip.core.customdev.util.Message
import java.util.HashMap

/**
 * Exemplo de estudo: listas em Groovy dentro do CPI
 */
def Message processData(Message message) {
    // --- 1. Criar a lista inicial ---
    def fruits = ["Apple", "Orange", "Grape"]

    // --- 2. Adicionar um novo item ---
    fruits.add("Banana")   // ["Apple", "Orange", "Grape", "Banana"]

    // --- 3. Remover item específico ---
    fruits.remove("Apple") // ["Orange", "Grape", "Banana"]

    // --- 4. Obter item pelo índice ---
    def thirdFruit = fruits[2]  // Resultado: "Banana"

    // --- 5. Salvar propriedades no contexto do CPI ---
    message.setProperty("fruitList", fruits)        // Lista completa
    message.setProperty("thirdFruit", thirdFruit)   // Apenas o item "Banana"

    // --- 6. Definir o Body ---
    // Se quiser mostrar o último item da lista de forma segura:
    def lastFruit = fruits[fruits.size() - 1]
    message.setBody("Última fruta da lista: ${lastFruit}" as String)

    // --- 7. Retornar mensagem modificada ---
    return message
}
