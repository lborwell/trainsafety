package jmri.jmrix.loconet.locomon;

import java.io.PrintWriter;
import java.net.Socket;
import java.util.Date;
import jmri.jmrix.loconet.LnTrafficController;
import jmri.jmrix.loconet.LnTrafficListener;
import static jmri.jmrix.loconet.LnTrafficListener.LN_TRAFFIC_ALL;
import jmri.jmrix.loconet.LocoNetListener;
import jmri.jmrix.loconet.LocoNetMessage;
import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;

public class HumeSender implements LnTrafficListener, LocoNetListener{
    LnTrafficController ct;
    jmri.jmrix.loconet.locomon.Llnmon llnmon = new jmri.jmrix.loconet.locomon.Llnmon();
    LocoNetSystemConnectionMemo m;
    Socket outSock;
    PrintWriter pw;
    
    public HumeSender(LocoNetSystemConnectionMemo m, Socket s){
        this.m = m;
        m.getLnTrafficController().addTrafficListener(LN_TRAFFIC_ALL, this);
        m.getLnTrafficController().addLocoNetListener(~0, this);
        
        outSock = s;
        
        try{
            pw = new PrintWriter(outSock.getOutputStream(),true);
        }catch(Exception e){e.printStackTrace();}
    }
    
    @Override
    public void notifyXmit(Date timestamp, LocoNetMessage m) {
    }

    @Override
    public void notifyRcv(Date timestamp, LocoNetMessage m) {
    }

    @Override
    public void message(LocoNetMessage msg) {
        try{
            String m = llnmon.format(msg);
            pw.println(m);
        }catch(Exception e){ e.printStackTrace(); }
    }
}
