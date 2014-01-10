import java.net.*;
import java.util.Scanner;
import java.io.*;

public class HumeReader{
    public static void main(String[] argv){
    	System.out.println("Begin");
    	HumeReader hr = new HumeReader();
    }

    public HumeReader(){
        try{
            Socket conn;
            conn = new Socket("localhost",55555);
            (new Thread(new Sendah(conn))).start();
            conn = new Socket("localhost",55555);
            (new Thread(new Receivah(conn))).start();
        }catch(Exception e){ e.printStackTrace(); }
    }
    
    class Receivah implements Runnable{
        Socket s;
        BufferedReader br;

        public Receivah(Socket s){
            this.s = s;

            try{
                br = new BufferedReader(new InputStreamReader(s.getInputStream()));
            }catch(Exception e){ e.printStackTrace(); }
        }

        public void run(){
            while(true){
                try{
                    String inp = null;
                    while(inp == null)
                        inp = br.readLine();
                    System.out.println("Received: " + inp);
                }catch(Exception e){ e.printStackTrace(); }
            }
        }
    }
    
    class Sendah implements Runnable{
        Socket s;
        PrintWriter pw;

        public Sendah(Socket s){
            this.s = s;

            try{
                pw = new PrintWriter(s.getOutputStream(), true);
            }catch(Exception e){ e.printStackTrace(); }
        }

        public void run(){
            Scanner s = new Scanner(System.in);
            while(true){
                try{
                    String inp = s.nextLine();
                    pw.println(inp);
                    System.out.println("Sent message: " + inp);
                }catch(Exception e){
                    e.printStackTrace();
                }
            }
        }
    }
}
