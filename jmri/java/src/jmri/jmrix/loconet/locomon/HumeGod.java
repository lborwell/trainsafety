package jmri.jmrix.loconet.locomon;

import java.net.ServerSocket;
import java.net.Socket;
import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;

import java.util.HashMap;

/**
 *
 * @author Luke
 */ 
public class HumeGod {
    public HumeGod(LocoNetSystemConnectionMemo m){
        (new Thread(new ClientAccepter(m))).start();
    }
    
    class ClientAccepter implements Runnable {
        HumeTurnout[] turns;
        LocoNetSystemConnectionMemo m;
        ServerSocket serverSocket = null;
        
        public ClientAccepter(LocoNetSystemConnectionMemo m){
            turns = new HumeTurnout[4];
            turns[0] = new HumeTurnout(5,false,true,"B2");
            turns[1] = new HumeTurnout(6,false,false,"C1");
            turns[2] = new HumeTurnout(7,false,false,"A2");
            turns[3] = new HumeTurnout(8,false,true,"D1");
            
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
                    
                    (new Thread(new HumeListener(m,inSock,turns))).start();
                    HumeSender hs = new HumeSender(m,outSock,turns);
                }catch(Exception e){ e.printStackTrace(); }
            }
        }
    }
}
