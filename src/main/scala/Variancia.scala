    //GENEREICS E (CO/IN/CONTRA)VARIANCIA

trait Passageiro
class Pobre extends Passageiro
class ExtremaPobreza extends Pobre
class Rico extends Passageiro
class Iluminatis extends Rico

//classe transporte aceita transportar um tipo e seus sub-tipos (covariancia)
class Transporte[+P](_passageiros: P*)
{
  def passageiros: Seq[P] = _passageiros
}

object Transito {
  //o transporte por aceitar seus sub-tipos, tbm aceitará o rico ou pobre
  def prende(transporte: Transporte[Passageiro]): Unit = {
    println("@#$% de trânsito")
  }
  
  def naoprende(transporte: Transporte[Rico]): Unit = {
    println("to safe")
  }
}

val zePaulo: Pobre = new Pobre
val zeMoura: Pobre = new Pobre
val ninguem: ExtremaPobreza = new ExtremaPobreza

val buzao: Transporte[Passageiro] = new Transporte(zePaulo, zeMoura)
val aPe: Transporte[ExtremaPobreza] = new Transporte[ExtremaPobreza]

val billGates: Rico = new Rico
val steveJobs: Rico = new Rico
val ferrari: Transporte[Rico] = new Transporte(billGates, steveJobs)
val jatinho: Transporte[Iluminatis] = new Transporte[Iluminatis]

// Transito.prende(buzao)
// Transito.prende(ferrari)
// Transito.prende(aPe)
// Transito.naoprende(jatinho)


trait Tarefa
class TarefaFront extends Tarefa
class TarefaBack extends Tarefa

class Desenvolvedor[-T]
{
  def tarefa_feita(tarefas: T): Unit = {
    println("aguardando teste da tarefa")
  }
}

object Gerente{
  def atribuirTarefaBack(back_end: Desenvolvedor[TarefaBack], tarefa: TarefaBack): Unit = {
    back_end.tarefa_feita(tarefa)
  }
  
  def atribuirTarefaFront(front_end: Desenvolvedor[TarefaFront], tarefa: TarefaFront): Unit = {
    front_end.tarefa_feita(tarefa)
  }
}

val tornarSiteResponsivo: TarefaFront = new TarefaFront
val otimizarSQL: TarefaBack = new TarefaBack
val mexerNoCSS: TarefaFront = new TarefaFront
val refatorarControllers: TarefaBack = new TarefaBack
 
val desenvolvedorFront: Desenvolvedor[TarefaFront] = new Desenvolvedor[TarefaFront]
val desenvolvedorBack: Desenvolvedor[TarefaBack] = new Desenvolvedor[TarefaBack]
val desenvolvedorFullStack: Desenvolvedor[Tarefa] = new Desenvolvedor[Tarefa]

// Gerente.atribuirTarefaBack(desenvolvedorBack, otimizarSQL)
// Gerente.atribuirTarefaFront(desenvolvedorFront, tornarSiteResponsivo)
// Gerente.atribuirTarefaFront(desenvolvedorFullStack, mexerNoCSS)
// Gerente.atribuirTarefaBack(desenvolvedorFullStack, refatorarControllers)


trait Aeronave
class Aviao extends Aeronave
class Jato extends Aviao
class Helicop extends Aeronave

trait QuatroRodas
class Carro extends QuatroRodas
class Possante extends Carro
class Caminhao extends QuatroRodas

trait DuasRodas
class Moto extends DuasRodas
class Bros extends Moto
class Bike extends DuasRodas

class Veiculo[+V]

object Vrum{
  def voando(aeronave: Veiculo[Aeronave]): Unit = {
    println("voando, boy")
  }
  def voandoRazante(jato: Veiculo[Jato]): Unit = {
    println("razante")
  }
  
  def correndo(quatrorodas: Veiculo[QuatroRodas]): Unit = {
    println("correndo cá mulesta")
  }
  
  def empinando(duasrodas: Veiculo[DuasRodas]): Unit = {
    println("empinando e bolando")
  }
}

val avianca: Veiculo[Aviao] = new Veiculo[Aviao]
val helipoc: Veiculo[Helicop] = new Veiculo[Helicop]
val jato: Veiculo[Jato] = new Veiculo[Jato]
val fiat: Veiculo[Carro] = new Veiculo[Carro]
val honda: Veiculo[Moto] = new Veiculo[Moto]

// Vrum.voando(avianca)
// Vrum.correndo(fiat)
// Vrum.empinando(honda)
// Vrum.voando(helipoc)
// Vrum.voandoRazante(jato)

trait Tecido
class Macio extends Tecido
class Fofo extends Tecido
class Aspero extends Tecido

class Pano[-T]

object Trabalho{
  def passarPano(tecido: Pano[Aspero]): Unit = {
    println("pano passado")
  }
  def enxugarRosto(tecido: Pano[Fofo]): Unit = {
    println("rosto enxugadas")
  }
  def enxugarMaos(tecido: Pano[Macio]): Unit = {
    println("mãos enxugadas")
  }
}

val macio: Pano[Macio] = new Pano[Macio]
val fofo: Pano[Fofo] = new Pano[Fofo]
val aspero: Pano[Aspero] = new Pano[Aspero]
val pano: Pano[Tecido] = new Pano[Tecido]

// Trabalho.passarPano(pano)
// Trabalho.enxugarRosto(pano)
// Trabalho.enxugarMaos(pano)