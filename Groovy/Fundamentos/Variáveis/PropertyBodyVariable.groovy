import com.sap.gateway.ip.core.customdev.util.Message; 
import java.util.HashMap; 
    
def Message processData(Message message) {
    
    // 1. Acessa o objeto de propriedades da mensagem
    def properties = message.getProperties();

    // 2. Lê uma propriedade da mensagem e a armazena em uma variável local
    // Suponha que a propriedade 'nomeUsuario' já foi definida em um passo anterior
    def userName = properties.get("nomeUsuario");

    // 3. Lê o corpo da mensagem atual
    def originalBody = message.getBody(java.lang.String);

    // 4. Concatena o valor da variável local no corpo original
    def newBody = originalBody + "\n\n" + "O usuário é: " + userName;

    // 5. Define o novo corpo da mensagem
    message.setBody(newBody);

    // 6. Adiciona uma nova propriedade para uso futuro
    message.setProperty("statusProcessamento", "Concluído com sucesso");

    return message;
}