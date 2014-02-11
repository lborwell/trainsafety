package jmri.jmrix.loconet.locomon;

import java.io.PrintWriter;
import java.net.Socket;
import java.util.Date;
import java.util.HashMap;
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
    
    HashMap<String,String> sensors;
    HashMap<Integer,HumeTurnout> turns = new HashMap<Integer,HumeTurnout>();

    Pattern speedp;
    Pattern sensorp;
    Pattern dirp;
    Pattern turnp;
    
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
    
    
    Set loco in slot 9 direction to REV, F0=Off, F1=On,  F2=Off, F3=Off, F4=Off.
    Set loco in slot 9 direction to FWD, F0=Off, F1=On,  F2=Off, F3=Off, F4=Off.
    
    
    Requesting Switch at LT14 () to Closed (output On).
    Requesting Switch at LT14 () to Closed (output On).
    Requesting Switch at LT5 () to Thrown (output On).
    LONG_ACK: Switch request Failed!
    Requesting Switch at LT6 () to Thrown (output On).
    Requesting Switch at LT5 () to Thrown (output On).
    Requesting Switch at LT7 () to Thrown (output On).
    Requesting Switch at LT8 () to Thrown (output On).
    */
    
    
    public HumeSender(LocoNetSystemConnectionMemo m, Socket s, HumeTurnout[] t){
        speedp = Pattern.compile(".*Set speed of loco in slot (\\d+) to (\\d+).*");
        sensorp = Pattern.compile(".*Sensor (\\d+) \\(\\) is (Hi|Lo).*");
        dirp = Pattern.compile(".*Set loco in slot (\\d+) direction to (FWD|REV).*");
        turnp = Pattern.compile(".*Requesting Switch at (\\d+) \\(\\) to (Thrown|Closed).*");
        
        sensors = new HashMap<String,String>();
        sensors.put("1953", "A1");
        sensors.put("1954", "A2");
        sensors.put("1957", "B1");
        sensors.put("1958", "B2");
        sensors.put("1961", "C1");
        sensors.put("1962", "C2");
        sensors.put("1965", "D1");
        sensors.put("1966", "D2");
        
        for(HumeTurnout ht : t)
            turns.put(ht.id, ht);

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
        m = m.trim();
        
        try{
            Matcher match = speedp.matcher(m);
            if(match.matches()){
                pw.println("speed " + match.group(1) + " " + match.group(2));
                return;
            }
            match = sensorp.matcher(m);
            if(match.matches()){
                pw.println("sensor " + match.group(2) + " " + sensors.get(match.group(1)));
                return;
            }
            match = dirp.matcher(m);
            if(match.matches()){
                pw.println("dir " + match.group(1) + " " + (match.group(2).equals("FWD")? "fwd" : "bkw"));
                return;
            }
            match = turnp.matcher(m);
            if(match.matches()){
                HumeTurnout t = turns.get(Integer.parseInt(match.group(1)));
                if(t==null) return;
                if(t.sendMsg(match.group(2).equals("Thrown")))
                    pw.println(t.toString());
            }
        }catch (Exception e){ e.printStackTrace(); }
    }
}
