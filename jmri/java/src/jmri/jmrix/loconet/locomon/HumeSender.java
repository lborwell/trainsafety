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

import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class HumeSender implements LnTrafficListener, LocoNetListener{
    LnTrafficController ct;
    jmri.jmrix.loconet.locomon.Llnmon llnmon = new jmri.jmrix.loconet.locomon.Llnmon();
    LocoNetSystemConnectionMemo m;
    Socket outSock;
    PrintWriter pw;

    Pattern speedp;
    Pattern sensorp;
    
    /*
    A1: LS1953
    A2: LS1954
    B1: LS1957
    B2: LS1958
    C1: LS1961
    C2: LA1962
    D1: LS1965
    D2: LS1966
    
    Received: Set speed of loco in slot 9 to 113.
    Received:
    Received: Set speed of loco in slot 9 to 113.
    Received:
    Received: Transponder address 2 (short) present at 1959 () (BDL16x Board 123 RX4
    zone D).
    Received:
    Received: Sensor 1965 () is Hi.  (BDL16 #123, DS13; DS54/64 #246, AuxC/A3)
    Received:
    Received: Transponder address 2 (short) absent at 1953 () (BDL16x Board 123 RX4
    zone A).
    Received:
    Received: Sensor 1953 () is Lo.  (BDL16 #123, DS1; DS54/64 #245, AuxA/A1)
    Received:
    */
    
    
    public HumeSender(LocoNetSystemConnectionMemo m, Socket s){
        speedp = Pattern.compile("Set speed of loco in slot (\d+) to (\d+)(.*)");
        sensorp =  = Pattern.compile("Sensor (\d+) () is (Hi|Lo)(.*)");

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
            sendMsg(m);
            //pw.println(m);
        }catch(Exception e){ e.printStackTrace(); }
    }

    private void sendMsg(String m){
        try{
            Matcher m = speedp.matcher(m);
            if(m.matches()){
                pw.println("speed " + m.group(1) + " " + m.group(2));
                return;
            }
            m = sensorp.matcher(m);
            if(m.matches()){
                pw.println("sensor " + m.group(2) + " " + m.group(1));
                return;
            }
        }catch (Exception e){ e.printStackTrace(); }
    }
}
