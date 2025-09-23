import com.sap.gateway.ip.core.customdev.util.Message; 
import java.util.HashMap; 

def Message processData(Message message) {
    
    // --- 1. Definição de Variáveis ---
    // Utilizando a palavra-chave 'def' do Groovy para definir variáveis de forma dinâmica.
    def nome = "Hideki"
    def n1 = 2
    def n2 = 2

    // --- 2. Realizando Operações Aritméticas ---
    def soma = n1 + n2
    def subtracao = n1 - n2
    def divisao = n1 / n2
    def multiplicacao = n1 * n2

    // --- 3. Montando o Corpo da Mensagem (Payload) ---
    // Uso de `GStrings` (Strings do Groovy com interpolação de variáveis) para um código mais limpo e legível.
    // Isso evita a concatenação manual com o sinal '+'.
    def body = """
Seu Nome: ${nome}
Soma: ${soma}
Subtração: ${subtracao}
Divisão: ${divisao}
Multiplicação: ${multiplicacao}
               """
    // O uso de aspas triplas `"""` permite que o texto seja formatado em várias linhas, mantendo a estrutura original.

    // --- 4. Modificando a Mensagem e Retornando ---
    // Define o novo corpo da mensagem `message` com o conteúdo formatado na variável `body`.
    message.setBody(body as String)

    // Retorna o objeto `message` modificado, que será o resultado final do script.
    return message
}