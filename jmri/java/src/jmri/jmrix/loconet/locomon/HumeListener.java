package jmri.jmrix.loconet.locomon;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.HashMap;
import java.util.Scanner;
import jmri.jmrit.roster.Roster;
import jmri.jmrit.roster.RosterEntry;
import jmri.jmrix.loconet.LnTrafficController;
import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;
import jmri.jmrix.loconet.LocoNetThrottle;
import jmri.jmrix.loconet.SlotManager;

public class HumeListener implements Runnable{
    LnTrafficController ct;
    jmri.jmrix.loconet.locomon.Llnmon llnmon = new jmri.jmrix.loconet.locomon.Llnmon();
    SlotManager sm;
    HashMap<Integer,Locomotive> locos;
    Roster roster;
    LocoNetSystemConnectionMemo m;
    Socket inSock;
    
    HashMap<Integer,Integer> slottoaddr = new HashMap<Integer,Integer>();
    HashMap<String,HumeTurnout> turns = new HashMap<String,HumeTurnout>();

    BufferedReader instream;
    public HumeListener(LocoNetSystemConnectionMemo m, Socket s, HumeTurnout[] t){
        slottoaddr.put(9,2);
        slottoaddr.put(8,1);
 
        inSock = s;
        
        for(HumeTurnout ht : t)
            turns.put(ht.trackID + " " + ht.dirString(), ht);
        
        try{
            instream = new BufferedReader(new InputStreamReader(inSock.getInputStream()));
        }catch(Exception e){ e.printStackTrace(); }
        
        this.m = m;
        sm = m.getSlotManager();
        locos = new HashMap<Integer,Locomotive>();
        getTrains();
    }
    
    public void getTrains(){
        roster = Roster.instance();
        for(int i=0; i<roster.numEntries(); i++){
            RosterEntry re = roster.getEntry(i);
            int addr = Integer.parseInt(re.getDccAddress());
            //int addr = re.getDccLocoAddress();
            Locomotive l = new Locomotive(m,addr,this,re);
            locos.put(Integer.valueOf(addr), l);
            sm.slotFromLocoAddress(addr, l);
        }
        printRoster();
    }
    
    void printRoster(){
        for(int k : locos.keySet())
            System.out.println("Key: " + k);
        //Locomotive l = locos.get(2);
    }
    
    @Override
    public void run() {
        while(true){
            String rec = null;
            try{
                while(rec == null)
                    rec = instream.readLine();
                
                Scanner s = new Scanner(rec);
                s.useDelimiter(" ");
                
                int i = s.nextInt();
                if(i == 0){
                    //train
                    int c = s.nextInt();
                    Locomotive l = locos.get(addrFromSlot(c));
                    LocoNetThrottle t = getThrottle(l);
                    setSpeed(l,t,s.nextInt());
                }else if(i==1){
                    //reverse train
                    String dir = s.next();
                    Locomotive l = locos.get(addrFromSlot(Integer.valueOf(s.nextInt())));
                    LocoNetThrottle t = getThrottle(l);
                    t.setIsForward(dir.equals("fwd"));
                }else if(i==2){
                    //turnout
                    HumeTurnout ht = turns.get(s.next() + " " + s.next());
                    ht.set(HumeTurnout.setToBool(s.next()));
                }
            }catch(Exception e){ e.printStackTrace(); }
            rec = null;
        }
    }
    
    LocoNetThrottle getThrottle(Locomotive l){
        LocoNetThrottle t = null;
        while(t==null)
            t = l.getThrottle();
        return t;
    }

    void setSpeed(Locomotive l, LocoNetThrottle t, int speed){
        //Attempt to stop hardware from ignoring speed message
        //while(t.getLocoNetSlot().speed() != speed)
        if(t==null) return;
        //while(t.getSpeedSetting() != t.floatSpeed(speed))
        t.setSpeedSetting(t.floatSpeed(speed));
    }
    
    int addrFromSlot(int i){
        return slottoaddr.get(i);
    }
    
}
