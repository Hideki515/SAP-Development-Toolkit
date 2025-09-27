import com.sap.gateway.ip.core.customdev.util.Message   // Importa a classe Message usada em scripts do SAP CPI
import groovy.xml.MarkupBuilder                         // Importa a classe para construir XML (não é usada nesse exemplo)

// 1. Definição da função principal
def Message processData(Message message) {
    
    def status = message.getProperty("Status");
    def valor = "120.50";
    
    // Chama a função auxiliar para processar a informação
    def log1 = createLogEntry("Pagamento", status, valor);
    
    // Suponha que, mais tarde, o status mude
    status = "CONCLUÍDO";
    
    // Chama a função auxiliar novamente (reutilização!)
    def log2 = createLogEntry("Finalização", status, valor);
    
    message.setBody(log1 + "\n" + log2);
    
    return message;
}

// 2. Definição da Função Auxiliar
// Esta função recebe 3 parâmetros e retorna uma String formatada
def createLogEntry(String type, String status, String value) {
    // Retorna a string formatada
    return "LOG [" + type + "] - Status: " + status + " - Valor: " + value;
}