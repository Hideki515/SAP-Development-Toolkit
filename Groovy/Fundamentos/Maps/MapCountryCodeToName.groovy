import com.sap.gateway.ip.core.customdev.util.Message
import java.util.HashMap

import com.sap.gateway.ip.core.customdev.util.Message
import java.util.HashMap

// Função principal que o SAP CPI chama ao executar o script
def Message processData(Message message) {
    
    // 1. Cria um mapa estático que funciona como tabela de tradução
    //    - A chave é o código do país (ISO Alpha-2)
    //    - O valor é o nome completo do país
    def countryLookup = [
        "BR": "Brazil",
        "US": "United States",
        "DE": "Germany",
        "JP": "Japan"
    ];
    
    // 2. Lê do Header da mensagem o código do país enviado pela aplicação chamadora
    //    Exemplo: "BR"
    def countryCode = message.getHeaders().get("Sender_Country_Code"); 
    
    // 3. Valor padrão caso não seja possível encontrar a tradução
    def fullCountryName = "UNKNOWN";

    // 4. Se o código não for nulo e existir no mapa de tradução...
    if (countryCode != null && countryLookup.containsKey(countryCode)) {
        // 5. Busca no mapa o nome completo correspondente ao código
        fullCountryName = countryLookup.get(countryCode); 
    }

    // 6. Grava um novo Header na mensagem com o nome completo do país traduzido
    //    Esse header pode ser usado depois em outros passos do iFlow
    message.setHeader("Country_Name_Full", fullCountryName);

    // 7. Retorna a mensagem para o fluxo continuar
    return message;
}
