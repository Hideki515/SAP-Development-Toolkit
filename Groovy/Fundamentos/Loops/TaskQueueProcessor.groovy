import com.sap.gateway.ip.core.customdev.util.Message   // Importa a classe Message usada em scripts do SAP CPI
import groovy.xml.MarkupBuilder                         // Importa a classe para construir XML (não é usada nesse exemplo)

def Message processData(Message message) {
    
    // Cria uma lista representando a fila de tarefas pendentes
    def taskQueue = ["Validar Dados", "Aplicar Regras", "Gerar Saída"]
    
    // String que vai acumular os logs do processamento
    def processLog = "Iniciando Processamento da Fila...\n"
    
    // Estrutura de repetição while: continua até que a lista esteja vazia
    while (!taskQueue.isEmpty()) {
        
        // remove(0) remove e retorna o primeiro elemento da lista
        // isso faz a fila andar para frente
        def currentTask = taskQueue.remove(0)
        
        // Concatena no log a tarefa concluída
        processLog += "Tarefa concluída: " + currentTask + "\n"
        
        // OBS: como sempre removemos o primeiro elemento, o loop vai acabar quando a lista estiver vazia
    }
    
    // Ao sair do while, a lista está vazia, então finalizamos o log
    processLog += "Fila de tarefas vazia. Fim."
    
    // Define o corpo da mensagem com o log completo
    message.setBody(processLog)
    
    // Retorna a mensagem modificada para o CPI continuar o fluxo
    return message
}
