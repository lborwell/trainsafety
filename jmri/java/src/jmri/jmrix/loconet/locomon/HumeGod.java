package jmri.jmrix.loconet.locomon;

import java.net.ServerSocket;
import java.net.Socket;
import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;

/**
 *
 * @author Luke
 */ 
public class HumeGod {
    public HumeGod(LocoNetSystemConnectionMemo m){
        (new Thread(new ClientAccepter(m))).start();
    }
    
    class ClientAccepter implements Runnable {
        LocoNetSystemConnectionMemo m;
        ServerSocket serverSocket = null;
        
        public ClientAccepter(LocoNetSystemConnectionMemo m){
            this.m = m;
            try{
                serverSocket = new ServerSocket(55555);
            }catch(Exception e){ e.printStackTrace(); }
        }
        
        public void run(){
            while(true){
                try{
                    Socket inSock = serverSocket.accept();
                    Socket outSock = serverSocket.accept();
                    
                    (new Thread(new HumeListener(m,inSock))).start();
                    HumeSender hs = new HumeSender(m,outSock);
                }catch(Exception e){ e.printStackTrace(); }
            }
        }
    }
}
