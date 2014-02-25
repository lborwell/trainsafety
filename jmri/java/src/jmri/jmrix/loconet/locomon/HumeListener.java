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
                
                System.out.println("Rec: " + rec);
                
                process(rec);
            }catch(Exception e){ e.printStackTrace(); }
            rec = null;
        }
    }

    public void process(String rec){
        try{
                Scanner s = new Scanner(rec);
                s.useDelimiter(" ");
                
                int i = s.nextInt();
                if(i == 0){
                    //train
                    setLocoSpeed(s);
                }else if(i==1){
                    //reverse train
                    setLocoDirection(s);
                }else if(i==2){
                    //turnout
                    setTurnout(s);
                }else if(i==3){
                    //delayed action
                    String ns = "";
                    while(s.hasNext()) ns += s.next() + " ";
                    new Thread(new Wait(this,ns)).start();
                }
        }catch(Exception e){ e.printStackTrace(); }
    }

    synchronized void setLocoSpeed(Scanner s){
        int c = s.nextInt();
        Locomotive l = locos.get(addrFromSlot(c));
        LocoNetThrottle t = getThrottle(l);
        setSpeed(l,t,s.nextInt());
    }

    synchronized void setLocoDirection(Scanner s){
        Locomotive l = locos.get(addrFromSlot(Integer.valueOf(s.nextInt())));
        String dir = s.next();
        LocoNetThrottle t = getThrottle(l);
        t.setIsForward(dir.equals("fwd"));
    }

    synchronized void setTurnout(Scanner s){
        HumeTurnout ht = turns.get(s.next() + " " + s.next());
        ht.set(HumeTurnout.setToBool(s.next()));
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

    private class Wait implements Runnable{
        HumeListener l; String s;
        public Wait(HumeListener l, String s){ this.l = l; this.s = s; }
        public void run(){ try{ Thread.sleep(5000); }catch(Exception e){ e.printStackTrace(); } l.process(s); }
    }
    
}
