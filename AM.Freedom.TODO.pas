unit AM.Freedom.TODO;

interface


{20/08/2013 -oAlexandro -cTestes : Testar TCustomEntity }
{21/08/2013 -oAlexandro -cTestes : Testar TCustomEntityList }
{22/08/2013 -oAlexandro -cPersistencia : Verificar caso o ColumnAtrtibute não seja nullable e enviar exceção }
{22/08/2013 -oAlexandro -cTCriteria : Adicionar o comparador IN(v1, v2, v3, v4) que testa o Variant como um array }
{23/08/2013 -oAlexandro -cSearchCriteria : Implementar TRttiCriteria[TRttiCriteria = class(TCustomCriteria)] } //deprecated;
{24/08/2013 -oAlexandro -cTSearchCriteria implementar ICriteriaTextGenerator(classes para gerar os textos conforme os valores dos criterios)
{24/08/2013 -oAlexandro -cTCustomEntity : Adicionar comando Persist(pObjectState: TObjectState[osClean, osInserted, osDeleted]; pFreeOnTerminate: Boolean = False) }
{25/08/2013 -oAlexandro -cBlobColumn : Implementar que TStrings posam ser lidas e persistidas }
{29/08/2013 -oAlexandro -cAttributes : Implementar attribute que fara com que a coluna não seja persistida (NoUpdate, NoInsert) }
{29/08/2013 -oAlexandro -cAttributes : Implementar attribute ID }
{31/08/2013 -oAlexandro -cTriggers : Implementar processo de Triggers de before e after(Insert, Update, Delete)}
{01/09/2013 -oAlexandro -cID_Sequence : Implementar forma de indicar que a sequence é auto inc(campos auto incremento)}
{02/09/2013 -oAlexandro -cMemoryLeaks : Implementar forma de eliminar os objetos criados sem interação com o usuario final}
{02/09/2013 -oAlexandro -cTCustomEntityList/TCustomEntity : Implementar leitura de ligações multiplas, ex: uma lista(TEntityList) dentro de um objeto simples(TEntity)}
{02/09/2013 -oAlexandro -cAttributes : Implementar attribute que fara com que a coluna não seja excluida (NoDelete) }
{02/09/2013 -oAlexandro -cTCustomEntityList/TCustomEntity : Implementar Requerimentos coRequired para ListColumn e EntityColumn }
{03/09/2013 -oAlexandro -cTCustomEntityList/TCustomEntity : Implementar persistencia de ligações multiplas, ex: uma lista(TEntityList) dentro de um objeto simples(TEntity) }
{04/09/2013 -oAlexandro -cTLinker : Implementar TLinker<T: Class>: iniciado 26/08/2013 }
{04/08/2013 -oAlexandro -cILinkerTextGenerator : Implementar ILinkerTextGenerator: iniciado 27/08/2013 }
{09/09/2013 -oAlexandro -cLazyLoad : Implementar Metodos para LazyLoad(Carga Tardia) - classe que fara a carga tardia dos TObject simples - iniciado 06/09/2013}
{09/09/2013 -oAlexandro -cMemoryLeaks : Refactoring}
{10/09/2013 -oAlexandro -cLazyLoad : Implementar Metodos para LazyLoad(Carga Tardia) - classe que fara a carga tardia dos TObject listas - iniciado 06/09/2013}
{12/09/2013 -oAlexandro -cVirtualMethodControl : Implementar Attribute MethodControl que recebera uma classe(IMethodControl) que contera metodos pra controle de execução do metodo mapeado na classe}


{12/01/2014 -oAlexandro -cLazy<T: class> : Adicionar as classes para LazyLoad}
{13/01/2014 -oAlexandro -cSetLazySearch : Ajustar o metodo SetLazySearch}
{13/01/2014 -oAlexandro -cRefactoring : Refatorar a Class TObjectWriter}
{15/01/2014 -oAlexandro -cEntity : Testar o atribute entity no object reader}
{16/01/2014 -oAlexandro -cColumnMapper.ColumnValue : Realizar o preenchimento correto da property ColumnValue quando for ler(ObjectReader) o objeto}
{20/01/2014 -oAlexandro -cPersistent : Implementar Persistents, como exemplo EntityORM}
{07/02/2014 -oAlexandro -cFreedomObject.PopulateObjectMapper : Criar classe que pega um TPersistentCursor e preenche um TObjectMapper(TCursorReader)}
{07/02/2014 -oAlexandro -cFreedomObject.Persist : Criar uma classe que pega uma ObjectMapper, um InitialObjectMapper um ObjetoState e um TObject e retorna um TPersistentCursor(TCursorWriter)}
{07/02/2014 -oAlexandro -cDUnit : Implementar tests de Performance dos DBPersistents(Carga, inserts, updates e deletes) de n registros testar ate 10mil registros em unica vez}
{07/02/2014 -oAlexandro -cDUnit : Implementar testes de Delete do TFreedomObject}
{09/02/2014 -oAlexandro -cDUnit : Implementar testes da TFreedomObjectList}
{09/02/2014 -oAlexandro -cPersistent : Persistir os detalhes colocando o valor do ParentID conforme IDColumn do pai}
{09/02/2014 -oAlexandro -cPersistent : Ao persistir os detalhes com um delete no pai, deletar os detalhes, se for lazy, faz a carga e deleta tudo}
{11/02/2013 -oAlexandro -cDUnit : Realizar testes de performance, esta demorando 27s para trazer 5000 registros do banco(baixou para 13s)}
{16/02/2014 -oAlexandro -cPersitent : Realizar a persistencia dos campos da tabela, ou seja, criar forma de enviar ao persistente que atualize o local de armazenamento conforme o que esta no objeto}
{18/02/2014 -oAlexandro -cPersistent : Ao persistir o objeto em insert ou update devera verificar o tamanho dos campos String e char(Copy ate o tamanho especificado)}
{18/02/2014 -oAlexandro -cMemoryLeaks : Verificar que estao sobrando 3 ObjectMappers}
{19/02/2014 -oAlexandro -cUpdateObject : Ao criar ou alterar uma tabela com base no objeto, adicionar a criação/alteração das chaves unicas}
{22/02/2014 -oAlexandro -cColumnType : Adicionar persistencia de TStreams, são tipos de colunas Blob, so que vao para um TStream e nao TStrings(verificar como ler o valor do blobStream(TBlobField))}
{22/02/2014 -oAlexandro -cPersistent : Realizar a persistencia de valores atraves de parametros permitindo assim que um campo blob binary possa ser persistido sem problemas}
{22/02/2014 -oAlexandro -cMemoryLeaks : Verificar que esta sobrando um TGenerateTextParams}
{22/02/2014 -oAlexandro -cFreedomObjectList : Adicionar busca de algum objeto da lista atraves de PropertyName, aceitar também propertyName.propertyname(busca em property de subclasse)}
{22/02/2014 -oAlexandro -cFreedomObjectList : Adicionar busca de algum objeto da lista detalhe atraves de PropertyName)}
{22/02/2014 -oAlexandro -cPersistent - Refactoring : Alterar as funcoes da Interface para que peçam um TObject e não um TObjectMapper}
{22/02/2014 -oAlexandro -cPersistent : Adicionar Persistent para DataSnap(ex: TDataSnapPersistent = class(TCustomPersistent))}
{23/02/2014 -oAlexandro -cAttributes : Adicionar possibilidades de uma coluna tipo Enumerada seja persistida como Char ou Byte}
{23/02/2014 -oAlexandro -cAttributes : Adicionar possibilidades de uma coluna tipo Boolean seja persistida como Boolean(anteriormente so ia como string ou byte)}
{23/02/2014 -oAlexandro -cCustomPersistent : Adicionar Flag para atualizacao do objeto no destino quando for utilizado, ou seja, se o objeto for persistido ele tenta fazer a atualizacao do destino}
{24/02/2014 -oAlexandro -cDBPersistent : Adicionar possibilidade de os DBPersistent receberem drivers externos(classes adapter), ou seja, uma forma de remover a dependencia do conector DBExpress}
{25/02/2014 -oAlexandro -cFireDacConnector : Desenvolver classes Adapter para conexao atraves de FireDac}
{01/03/2014 -oAlexandro -cAttributes : Desenvolver atribute que mapeara um Field de tipo simples que vira de outra classe o valor(JoinedColumnMapper)}
{02/03/2014 -oAlexandro -cTSQLLinker : Desenvolver testes unitários da classe}
{02/03/2014 -oAlexandro -cTSQLLinkerTextGenerator : Desenvolver o gerador de sql da classe TSQLLinker}
{02/03/2014 -oAlexandro -cTSQLLinkerTextGenerator : Desenvolver testes unitários da classe}
{02/02/2014 -oAlexandro -cAttributes : Desenvolver attribute que mapeara as chaves unicas}
{02/03/2014 -oAlexandro -cAttributes : Desenvolver attribute que mapeara as chaves primarias}
{03/03/2014 -oAlexandro -cMSSQLPersistent : Desenvolver DDL Extracter(TMSSQLDDLExtracter) para este persistent}
{03/03/2014 -oAlexandro -cMSSQLPersistent : Desenvolver testes unitários para o TMSSQLDDLExtracter}
{03/03/2014 -oAlexandro -cMSSQLPersistent : Desenvolver testes unitarios para este persistent}
{03/03/2014 -oAlexandro -cPGPersistent : Desenvolver DBPERSISTENT para postgre}
{03/03/2014 -oAlexandro -cPGPersistent : Desenvolver DDL Extracter(TPGDDLExtracter) para este persistent}
{03/03/2014 -oAlexandro -cPGPersistent : Desenvolver testes unitários para o TPGDDLExtracter}
{03/03/2014 -oAlexandro -cPGPersistent : Desenvolver testes unitarios para este persistent}
{04/03/2014 -oAlexandro -cDropObject(DDL) : ao persistir o objeto na base, dropar os detalhes primeiro}
{08/03/2014 -oAlexandro -cUpdateObject(DDL) : ao persistir o objeto na base, tentar persistir antes os objetos de join, depois atualizar os detalhes}
{08/03/2014 -oAlexandro -cAttributes : Desenvolver attribute que mapeara o schema que o objeto/campo/sequence esta/sera definido(a)}
{08/03/2014 -oAlexandro -cAttribute : Definir attribute que mapeara os Domains}
{08/03/2014 -oAlexandro -cDBPersistent : Ao atualizar um campo com Domain mapeado, persistir os campo com o DataType(Domain)}
{08/03/2014 -oAlexandro -cDDLExtracter : Ajustar os DDLExtracters para buscar os DOmains e os fields mapeados com domain}
{08/03/2014 -oAlexandro -cDDLExtracter : Ajustar os DDLExtracters para buscar os Schemas configurados no banco}
{08/03/2014 -oAlexandro -cSchemaCommands : Desenvolver comandos que Criaram e excluirao os schemas no banco}
{15/03/2014 -oAlexandro -cCustomCommands : Ao mapear os schemas, executar os commandos tb conforme schema}
{16/03/2014 -oAlexandro -cCustomPersistent : Desenvolver uma PoolList de Objectos, onde os objectos podem ser Buscados no persitente, que os armazena e os libera conforme uso}
{16/03/2014 -oAlexandro -cCustomPersistent : Desenvolver forma de buscar os objectos na pool atraves de PropertyNames(prop.prop.prop...)}
{17/03/2014 -oAlexandro -cObjectMapper : CVerificar que quando um object faz referencia para si mesmo, levanta uma exceção de referencia multipla no from(Alias da tabela duplicado devido a referencia circular)}
{17/03/2014 -oAlexandro -cCustomPersistent, ObjectMapper : Adicionar possibilidade de nao mapear a coluna LastUpdateTime}
{18/03/2014 -oAlexandro -cFreedomObjectBind : Desenvolver componente que recebe um TFreedomObject e gera uma lista de fields disponiveis para livebindings}
{19/03/2014 -oAlexandro -cLazy : Verificar que ao buscar um objecto com carga tardia de um objeto carregado atraves de carga tardia, nao passa no create}
{19/03/2014 -oAlexandro -cLazy : Verificar que quando um objecto simples com carga tardia de outro object simples, faz a carga do objeto, porém, o campo de ligacao esta sem valor, da access violation}
{29/03/2014 -oAlexandro -cNullable : Desenvolver classe base para preenchimento dos valores nullos de cada tipo de dado}

implementation

end.
