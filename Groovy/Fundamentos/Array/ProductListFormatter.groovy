import com.sap.gateway.ip.core.customdev.util.Message
import java.util.HashMap

/**
 * Exemplo de estudo: lista de mapas (produtos) no Groovy do CPI
 * - Usa withIndex() para numerar os produtos
 */
def Message processData(Message message) {
    
    // --- 1. Criar lista de produtos ---
    def products = [
        [id: 1, name: "Laptop",  price: 1200],
        [id: 2, name: "Mouse",   price: 25],
        [id: 3, name: "Keyboard", price: 75]
    ]

    // --- 2. Montar as linhas processadas ---
    def lines = products.withIndex().collect { product, idx ->
        "Produto ${idx + 1}: ${product.name}, Preço: \$${product.price}"
    }

    // --- 3. Juntar em um único Body ---
    def newBody = lines.join("\n")

    // --- 4. Definir no Body da mensagem ---
    message.setBody(newBody)

    return message
}
