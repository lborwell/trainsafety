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

    BufferedReader instream;
    public HumeListener(LocoNetSystemConnectionMemo m, Socket s){
        slottoaddr.put(9,2);
        slottoaddr.put(8,1);
        
        
        inSock = s;
        
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
                    setSpeed(t,s.nextInt());
                }else if(i==1){
                    //reverse train
                    Locomotive l = locos.get(addrFromSlot(Integer.valueOf(s.nextInt())));
                    LocoNetThrottle t = getThrottle(l);
                    t.setIsForward(!t.getIsForward());
                }else if(i==2){
                    //turnout
                }
            }catch(Exception e){ e.printStackTrace(); }
            System.out.println("Received: " + rec);
            rec = null;
        }
    }
    
    LocoNetThrottle getThrottle(Locomotive l){
        if(l==null) System.out.println("l null");
        LocoNetThrottle t = null;
        while(t==null)
            t = l.getThrottle();
        return t;
    }

    void setSpeed(LocoNetThrottle t, int speed){
        //Attempt to stop hardware from ignoring speed message
        //while(t.getLocoNetSlot().speed() != speed)
            t.setSpeedSetting(t.floatSpeed(speed));
    }
    
    int addrFromSlot(int i){
        return slottoaddr.get(i);
    }
}
