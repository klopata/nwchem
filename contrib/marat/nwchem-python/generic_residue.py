'''
Created on Feb 5, 2012

@author: marat
'''
import sys
from generic_atom import *

class GenericResidue(object):
    '''
    classdocs
    '''


    def __init__(self,atoms=None):
        '''
        Default constructor for Residue class
        atoms list of atoms in the residue
        name residue name
        '''
        if atoms:
            self.atoms = atoms
        else:
            self.atoms=[]
            
    @classmethod        
    def fromPDBfile(cls,filename):
        '''
        alternative constructor from PDB file
        '''
        cls = GenericResidue()
        fp = open(str(filename),'r')
        
        for line in fp.readlines():
            if line.startswith('ATOM'):
                a=GenericAtom.fromPDBrecord(line)
                cls.AddAtom(a)
        fp.close
        return cls            

    def AddAtom(self,a):
        self.atoms.append(a)
        
    def delAtom(self,a):
        self.atoms.remove(a)
        
    def __str__(self):
        output = ""
        for a in self.atoms:
            output = output + str(a) + "\n"
        return output

    def connectAtoms(self):
        for i in range(len(self.atoms)):
            for j in range(i+1,len(self.atoms)):
                a1=self.atoms[i]
                a2=self.atoms[j]
                if GenericAtom.bonded(a1, a2):
                    a1.setBond(j)
                    a2.setBond(i)
                    
    def get_bonded(self,a0,elem=None):
        al =[]
        for a in self.byFilter():
            print GenericAtom.bonded(a, a0),GenericAtom.bondlength(a, a0)
            if a!=a0 and GenericAtom.bonded(a, a0):
                al.append(a)
        return al
                                    
    @staticmethod
    def distance(res1,res2):
        rmin=100
        for a1 in res1.atoms:
            for a2 in res2.atoms:
                r = GenericAtom.bondlength(a1, a2)
                if r < rmin:
                    a1_min = a1
                    a2_min = a2
                    rmin = r
        return rmin,a1_min,a2_min
    
    @staticmethod
    def hbonded(res1,res2):
        rOH=2.0
        OHO=143
        (r,a1,a2)=GenericResidue.distance(res1, res2)
        if r > rOH:
            return r, False
        if a1.elemName()=='H':
            res1,res2=res2,res1
            a1,a2=a2,a1
        elif a2.elemName()=='H':
            pass
        else:
            return False
        a3 = res2.get_bonded(a2, 'O')[0]
        angle = GenericAtom.angle(a1, a2, a3)
        return angle>OHO,r,angle
    
    @staticmethod
    def distance1(res1,res2):
        dr=100
        for a1 in res1.byFilter():
            for a2 in res2.byFilter():
                dr=min(dr,GenericAtom.bondlength(a1, a2))
                print dr,a1.name(),a2.name()
        return dr
        
    def byElement(self,name):
        for a in self.atoms:
            if a.elemName() is name:
                yield a

    def byFilter(self,elem=None):
        for a in self.atoms:
            if elem is None:
                yield a
            elif a.elemName() is elem:
                yield a
                            
    def size(self):
        return len(self.atoms)
    
if __name__ == '__main__':
#    aline1 = "ATOM      3  O2  IO3     1      -1.182   1.410   0.573       -0.80     O"
#    aline2 = "ATOM      1  I1  IO3     1      -1.555  -0.350   0.333        1.39     I"
#
#    res0 = GenericResidue()
#    print res0
#    a = GenericAtom.fromPDBrecord(aline2)
#    print a
#    res0.AddAtom(a)
#    print res0.size()
    
    res0 = GenericResidue.fromPDBfile("io3.pdb")
    print res0
 
    res1 = GenericResidue.fromPDBfile("h2o-1.pdb")
    print res1
    
    print "distance test"
    (r,a1,a2)=GenericResidue.distance(res0, res1)
    print r, a1.name(), a2.name()
    print res1.get_bonded(a2, "O")
    name = None
    print (filter(lambda a: name is None or a.elemName()==name,res1.atoms ))
    print GenericResidue.hbonded(res0,res1)
#    b = ResAtom.fromPDBrecord(aline1)
#    res0.AddAtom(a)
#    res0.AddAtom(b)
#    print res0.toPDBrecord(atom_start=1)