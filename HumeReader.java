import java.net.*;
import java.util.Scanner;

public class HumeReader{
    int portNum = 55555;
    DatagramSocket ss;
    
    public static void main(String[] argv){
    	System.out.println("Begin");
    	HumeReader hr = new HumeReader();
    }

    public HumeReader(){
        try{
         	ss = new DatagramSocket(portNum);
            (new Thread(new Sendah())).start();
            sendM();
        }catch(Exception e){ e.printStackTrace(); }
    }
    
    public void sendM() {
    	Scanner s = new Scanner(System.in);
    	while(true){
        	try{
        		byte[] inp = new byte[255];
                DatagramPacket inppac = new DatagramPacket(inp, inp.length);
                ss.receive(inppac);
                System.out.println("Received: " + new String(inp,0,inp.length));
        	}catch(Exception e){ e.printStackTrace(); }
        }
    }
    
    class Sendah implements Runnable{
        int port = 55557;
        DatagramSocket ss;

        public Sendah(){
            try{
                ss = new DatagramSocket(port);
            }catch(Exception e){ e.printStackTrace(); }
        }

        public void run(){
            Scanner s = new Scanner(System.in);
            while(true){
                try{
                    String inp = s.nextLine();
                    DatagramPacket dp = new DatagramPacket(inp.getBytes(),inp.length(),InetAddress.getLocalHost(),55556);
                    ss.send(dp);
                    System.out.println("Sent message: " + inp);
                }catch(Exception e){
                    e.printStackTrace();
                }
            }
        }
    }
}
