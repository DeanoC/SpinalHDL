import random
from Queue import Queue

import cocotb
from cocotb.triggers import Timer, RisingEdge

from spinal.common.Axi4 import Axi4
from spinal.common.Stream import StreamDriverSlave, StreamDriverMaster, StreamTransaction, StreamMonitor
from spinal.common.misc import ClockDomainAsyncReset, simulationSpeedPrinter, randBits, BoolRandomizer, assertEquals


class MasterHandle:
    def __init__(self,id):
        self.id = id
        self.counter = 0
        self.doFinish = False
        self.monitorQueues = [Queue() for i in xrange(4)] # One queue for each transaction id

    def isCompleted(self):
        if not self.doFinish:
            return False
        for q in self.monitorQueues:
            if not q.empty():
                return False
        return True

    def genRandomAddress(self):
        while True:
            value = randBits(12)
            if (value >> 10) != self.id  and ((value >> 8) & 0x3) == self.id:
                return value

    def genReadCmd(self):
        if self.doFinish:
            return None
        idOffset = randBits(2)
        trans = StreamTransaction()
        trans.addr = self.genRandomAddress()
        trans.id = self.id*4 + idOffset #Each master can use 4 ID
        trans.region = randBits(4)
        trans.len = randBits(4)
        trans.size = randBits(3)
        trans.burst = randBits(2)
        trans.lock = randBits(1)
        trans.cache = randBits(4)
        trans.qos = randBits(4)
        trans.prot = randBits(3)

        trans.progress = 0
        self.monitorQueues[idOffset].put(trans)
        # print("Master START  %d %x" % (trans.id, trans.addr))
        return trans


    def onReadRsp(self, trans):
        queue = self.monitorQueues[trans.id - self.id*4]
        task = queue.queue[0]
        assertEquals(trans.data,task.addr + task.progress,"Readed value is wrong")
        task.progress += 1
        if task.progress == task.len + 1:
            # print("Master FINISH %d %x" % (task.id,task.addr))
            queue.get()
            self.counter += 1
            if self.counter > 200:
                self.doFinish = True



class SlaveHandle:
    def __init__(self,id):
        self.tasksQueues = [Queue()] * 64 # One queue of task for each transaction id
        self.genRandomizer = BoolRandomizer()
        self.id = id

    def getRandTaskList(self):
        tasksQueuesFiltred = [tasksList for tasksList in self.tasksQueues if not tasksList.empty()]
        if len(tasksQueuesFiltred) == 0:
            return None
        return random.choice(tasksQueuesFiltred)

    def genReadRsp(self):
        tasksQueue = self.getRandTaskList()
        if tasksQueue:
            if self.genRandomizer.get():
                task = tasksQueue.queue[0]
                trans = StreamTransaction()
                trans.data = task.addr + task.progress
                trans.resp = 0
                trans.id = task.id
                task.progress += 1
                if task.progress == task.len + 1:
                    trans.last = 1
                    tasksQueue.get()
                else:
                    trans.last = 0
                return trans


    def onReadCmd(self, trans):
        trans.progress = 0
        assertEquals(trans.addr >> 10, self.id, ":(")
        self.tasksQueues[trans.id].put(trans)




@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

    axiMasters = [Axi4(dut, "axiMasters_" + str(i)) for i in range(3)]
    axiSlaves = [Axi4(dut, "axiSlaves_" + str(i)) for i in range(4)]

    masterHandles = []

    # Instanciate master side
    for idx,axiMaster in enumerate(axiMasters):
        masterHandle = MasterHandle(idx)
        masterHandles.append(masterHandle)
        StreamDriverMaster(axiMaster.ar, masterHandle.genReadCmd, dut.clk, dut.reset)
        StreamDriverSlave(axiMaster.r, dut.clk, dut.reset)
        StreamMonitor(axiMaster.r, masterHandle.onReadRsp, dut.clk, dut.reset)

    # instanciate slave side
    for idx,axiSlave in enumerate(axiSlaves):
        axiSlave.r.payload.id <= 0
        slaveHandle = SlaveHandle(idx)
        StreamDriverSlave(axiSlave.ar,dut.clk,dut.reset)
        StreamDriverMaster(axiSlave.r, slaveHandle.genReadRsp, dut.clk, dut.reset)
        StreamMonitor(axiSlave.ar, slaveHandle.onReadCmd, dut.clk, dut.reset)

    # Run until completion
    while True:
        yield RisingEdge(dut.clk)
        done = True
        for handle in masterHandles:
            if not handle.isCompleted():
                done = False
        if done:
            break

    yield Timer(1000*10)

    dut.log.info("Cocotb test done")