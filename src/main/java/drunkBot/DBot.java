package drunkBot;


import java.util.Map;

public class DBot {
    Map<String, String> state;

    public void init(Map<String, String> params) {
        state = params;
    }
}
