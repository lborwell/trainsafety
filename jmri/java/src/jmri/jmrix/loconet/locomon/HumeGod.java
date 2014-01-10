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
        ServerSocket serverSocket = null;
        
        try{
            serverSocket = new ServerSocket(55555);
        }catch(Exception e){ e.printStackTrace(); }
        
        while(true){
            try{
                Socket inSock = serverSocket.accept();
                Socket outSock = serverSocket.accept();
                
                (new Thread(new HumeListener(m,inSock))).start();
                HumeSender hs = new HumeSender(m,outSock);
            }catch(Exception e){ e.printStackTrace(); }
        }
        
        //HumeListener hl = new HumeListener();
        //hl.init(m);
        //(new Thread(hl)).start();
        //HumeSender hs = new HumeSender();
        //hs.init(m);
    }
}
