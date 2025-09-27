import com.sap.gateway.ip.core.customdev.util.Message   // Importa a classe Message usada em scripts do SAP CPI
import groovy.xml.MarkupBuilder                         // Importa a classe para construir XML (não é usada nesse exemplo)

import com.sap.gateway.ip.core.customdev.util.Message   // Importa a classe Message usada em scripts do SAP CPI
import groovy.xml.MarkupBuilder                         // Importa a classe para construir XML (não é usada nesse exemplo)

def Message processData(Message message) {              // Função principal que o CPI executa (entrada: message)
    
    // Lista de valores que representam pedidos (obtida de alguma etapa anterior)
    def orderAmounts = [150.50, 200.00, 45.99, 30.00, -20]   // Cria uma lista de valores numéricos (pedidos em dinheiro)
    
    // 1. Chama a função auxiliar para calcular o total
    def totalAmount = calculateTotal(orderAmounts);     // Invoca a função calculateTotal, passando a lista, e guarda o retorno
    
    // 2. Define o corpo e a propriedade com o resultado
    message.setBody("Total de pedidos processados: "    // Substitui o conteúdo do Body da mensagem
                    + totalAmount);                     // Concatena o texto com o valor calculado
    message.setProperty("TotalPedidos", totalAmount);   // Cria/atualiza a propriedade "TotalPedidos" na mensagem
    
    return message;                                     // Retorna a mensagem processada para o fluxo do CPI
}

// Função Auxiliar: Calcula a soma total de uma lista
def calculateTotal(List amounts) {                      // Define a função que recebe uma lista de números
    
    def total = 0.00;                                   // Inicializa o acumulador com 0.00 (double)
    
    // Itera sobre cada valor na lista
    for (amount in amounts) {                           // Percorre a lista de valores (cada elemento vira "amount")
        // Converte o valor para Double e acumula no total
        total += amount.toDouble();                     // Converte para Double (caso não seja) e soma ao acumulador
    }
    
    // Retorna o valor total
    return total;                                       // Envia o total calculado de volta para quem chamou a função
}
