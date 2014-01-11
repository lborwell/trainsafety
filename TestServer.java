import java.net.ServerSocket;
import java.net.Socket;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class TestServer {

    public static void main(String[] args){
        TestServer ts = new TestServer();
    }

    public TestServer(){
        (new Thread(new ClientAccepter())).start();
    }
    
    class ClientAccepter implements Runnable {
        ServerSocket serverSocket = null;
        
        public ClientAccepter(){
            try{
                serverSocket = new ServerSocket(55555);
            }catch(Exception e){ e.printStackTrace(); }
        }
        
        public void run(){
            while(true){
                try{
                    Socket inSock = serverSocket.accept();
                    System.out.println("Connected");
                    
                    (new Thread(new TestListener(inSock))).start();
                }catch(Exception e){ e.printStackTrace(); }
            }
        }

        class TestListener implements Runnable{
            Socket sock;
            BufferedReader bf;

            public TestListener(Socket s){
                sock = s;
                try{
                    bf = new BufferedReader(new InputStreamReader(sock.getInputStream()));
                }catch(Exception e){ e.printStackTrace(); }
            }

            public void run(){
                while(true){
                    try{
                        System.out.println(bf.readLine());
                    }catch(Exception e){ e.printStackTrace(); }
                }
            }
        }
    }
}