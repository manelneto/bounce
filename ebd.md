# EBD: Componente de Especificação da Base de Dados

O ***Community Connect*** é um sistema de informação web-based que permite aos utilizadores partilharem perguntas e obterem respostas sobre diversos temas, tendo como intuito fornecer respostas para problemas comuns, num ambiente de interajuda solidária.

## A4: Modelo Conceptual de Dados

O modelo conceptual de dados - constituído pelo diagrama UML de classes e pelas regras de negócio - procura elencar e caracterizar os elementos significativos do sistema (classes) e a forma como eles se relacionam entre si (associações). Este modelo é o ponto de partida para o esquema relacional da base de dados.


### 1. Diagrama de Classes

O diagrama de classes do ***Community Connect*** encontra-se representado na Figura 1. Este diagrama inclui todas as entidades (classes) relevantes para a especificação da base de dados, as associações entre elas e a respetiva multiplicidade, bem como os seus atributos e os domínios/restrições correspondentes.

![Diagrama de Classes](uploads/EBD-A4-DiagramaClasses.png)

Figura 1 - Diagrama de Classes


### 2. Regras de Negócio Adicionais
 
As restrições e regras de negócio que não puderam ser incluídas no diagrama de classes elencam-se na Tabela 1.

| Identificador | Descrição |
| ------------- | --------- |
| BR01 | Nenhum utilizador autenticado (*User*) pode votar (*Vote*) numa publicação (*Post*) da qual é autor |
| BR02 | Se um utilizador autenticado (*User*) for apagado, o conteúdo do qual é autor mantém-se na base de dados com um autor anónimo |
| BR03 | O *rating* de um utilizador autenticado (*User*) numa comunidade (*Community*) é calculado de acordo com a fórmula 1000 x likes/(likes + dislikes) nas suas respostas (*Answer*) dentro dessa comunidade |
| BR04 | Um utilizador autenticado (*User*) é *expert* de uma comunidade (*Community*) se e só se tiver todos os emblemas (*Badge*) possíveis e um *rating* superior a 800 |
| BR05 | Os ficheiros (*file*) das publicações (*Post*) devem ter uma das seguintes extensões (ou seja, terminar em) *jpg*, *jpeg*, *png*, *txt*, *pdf*, *doc* |
| BR06 | Cada utilizador autenticado (*User*) só pode votar (*Vote*) em cada publicação (*Post*) uma vez

Tabela 1 - Regras de Negócio Adicionais


---


## A5: Esquema Relacional, Validação e Afinação

O esquema relacional - convertido a partir do modelo conceptual - mapeia as classes e associações em relações/tabelas, bem como os respetivos atributos e restrições. Quando devidamente validado e afinado, o esquema relacional garante a minimização/eliminação de anomalias, quer de redundância, quer de atualização/eliminação.


### 1. Esquema Relacional

O esquema relacional do ***Community Connect*** encontra-se, em notação compacta, na Tabela 2. Este inclui os esquemas das relações (tabelas), bem como os seus atributos e respetivas restrições (domínios, atributos chave primária, atributos chave externa e restrições de intregridade: NOT NULL, UNIQUE, DEFAULT e CHECK).  

| Referência da Relação | Notação Compacta da Relação |
| --------------------- | --------------------------- |
| R01 | user(<ins>id</ins>, username **NN** **UK**, email **NN** **UK**, password **NN**, register_date **NN** **DF** Today, administrator **NN** **DF** false, blocked **NN** **DF** false, image) |
| R02 | notification(<ins>id</ins>, content **NN**, date **NN** **DF** Today, read **NN** **DF** false, id_user → user **NN**) |
| R03 | badge(<ins>id</ins>, name **NN** **UK**) |
| R04 | user_earns_badge(<ins>id_user → user</ins>, <ins>id_badge → badge</ins>) |
| R05 | tag(<ins>id</ins>, name **NN** **UK**) |
| R06 | user_follows_tag(<ins>id_user → user</ins>, <ins>id_tag → tag</ins>) |
| R07 | community(<ins>id</ins>, name **NN** **UK**) |
| R08 | user_follows_community(<ins>id_user → user</ins>, <ins>id_community → community</ins>) |
| R09 | user_moderates_community(<ins>id_user → user</ins>, <ins>id_community → community</ins>) |
| R10 | reputation(<ins>id_user → user</ins>, <ins>id_community → community</ins>, rating **NN** **DF** 0 **CK** rating >= 0, expert **NN** **DF** false) |
| R11 | question(<ins>id</ins>, content **NN**, date **NN** **DF** Today, file, last_edited **CK** last_edited >= date, title **NN**, id_user → user **NN**, id_community → community **NN**) |
| R12 | user_follows_question(<ins>id_user → user</ins>, <ins>id_question → question</ins>) |
| R13 | question_tags(<ins>id_question → question</ins>, <ins>id_tag → tag</ins>) |
| R14 | question_vote(<ins>id_question → question</ins>, <ins>id_user → user</ins>, like **NN**) |
| R15 | question_comment(<ins>id</ins>, content **NN**, date **NN** **DF** Today, last_edited **CK** last_edited >= date, id_user → user **NN**, id_question → question **NN**) |
| R16 | answer(<ins>id</ins>, content **NN**, date **NN** **DF** Today, file, last_edited **CK** last_edited >= date, correct **NN** **DF** false, id_user → user **NN**, id_question → question **NN**) |
| R17 | answer_vote(<ins>id_answer → answer</ins>, <ins>id_user → user</ins>, like **NN**) |
| R18 | answer_comment(<ins>id</ins>, content **NN**, date **NN** **DF** Today, last_edited **CK** last_edited >= date, id_user → user **NN**, id_answer → answer **NN**) |

Tabela 2 - Esquema Relacional

Legenda:
- NN = NOT NULL
- UK = UNIQUE
- DF = DEFAULT
- CK = CHECK

A generalização das classes *Question* e *Answer* em *Post*, isto é, a especialização da classe *Post* em *Question* e *Answer*, foi convertida do modelo conceptual para o esquema relacional segundo uma abordagem orientada a objetos. De acordo com Jeffrey Ullman e Jennifer Widom em *A first course in Database Systems 3<sup>rd</sup> Edition* (Secção 4.8.2) e com https://www.cs.uct.ac.za/mit_notes/database/htmls/chp07.html, este método é o que mais se adequada a generalizações disjuntas e completas - como é o caso. Tal deve-se, principalmente, ao facto de - sendo a generalização disjunta e completa - o número de relações a serem criadas ser exatamente o número de subclasses, dado que nenhum elemento pode pertencer às duas subclasses em simultâneo (a generalização não seria disjunta) ou apenas à superclasse (a generalização não seria completa). Esta abordagem permite minimizar o espaço ocupado (evita-se a tabela para a superclasse) e facilita futuras consultas à base de dados, minimizando o número de junções necessárias para obter informação, em comparação com uma abordagem de mapeamento Entidade-Relação.


### 2. Domínios

Na Tabela 3, especifica-se o único domínio adicional.

| Nome do Domínio | Especificação do Domínio |
| --------------- | ------------------------ |
| Today	| DATE DEFAULT CURRENT_DATE |

Tabela 3 - Domínio Adicional


### 3. Validação do Esquema

A validação do esquema relacional é essencial para minimizar/eliminar anomalias de redundância e de atualização/eliminação. Nesse sentido, nas tabelas abaixo apresentam-se as dependências funcionais de todas as relações e a respetiva relação normalizada (na verdade, em todos os casos, a própria relação já está normalizada).

| **Tabela R01** | user |
| -------------- | ---- |
| **Chaves** | {id}, {username}, {email} |
| **Dependências Funcionais** |
| FD0101 | {id} → {username, email, password, register_date, administrator, blocked, image} |
| FD0102 | {username} → {id, email, password, register_date, administrator, blocked, image} |
| FD0103 | {email} → {id, username, password, register_date, administrator, blocked, image} |
| **Forma Normal** | BCNF |

Tabela 4 - Validação da Relação *user*

| **Tabela R02** | notification |
| -------------- | ------------ |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD0201 | {id} → {content, date, read, id_user} |
| **Forma Normal** | BCNF |

Tabela 5 - Validação da Relação *notification*

| **Tabela R03** | badge |
| -------------- | ----- |
| **Chaves** | {id}, {name} |
| **Dependências Funcionais** |
| FD0301 | {id} → {name} |
| FD0302 | {name} → {id} |
| **Forma Normal** | BCNF |

Tabela 6 - Validação da Relação *badge*

| **Tabela R04** | user_earns_badge |
| -------------- | ---------------- |
| **Chaves** | {id_user, id_badge} |
| **Dependências Funcionais** | *nenhuma* |
| **Forma Normal** | BCNF |

Tabela 7 - Validação da Relação *user_earns_badge*

| **Tabela R05** | tag |
| -------------- | --- |
| **Chaves** | {id}, {name} |
| **Dependências Funcionais** |
| FD0501 | {id} → {name} |
| FD0502 | {name} → {id} |
| **Forma Normal** | BCNF |

Tabela 8 - Validação da Relação *tag*

| **Tabela R06** | user_follows_tag |
| -------------- | ---------------- |
| **Chaves** | {id_user, id_tag} |
| **Dependências Funcionais** | *nenhuma* |
| **Forma Normal** | BCNF |

Tabela 9 - Validação da Relação *user_follows_tag*

| **Tabela R07** | community |
| -------------- | --------- |
| **Chaves** | {id}, {name} |
| **Dependências Funcionais** |
| FD0701 | {id} → {name} |
| FD0702 | {name} → {id} |
| **Forma Normal** | BCNF |

Tabela 10 - Validação da Relação *community*

| **Tabela R08** | user_follows_community |
| -------------- | ---------------------- |
| **Chaves** | {id_user, id_community} |
| **Dependências Funcionais** | *nenhuma* |
| **Forma Normal** | BCNF |

Tabela 11 - Validação da Relação *user_follows_community*

| **Tabela R09** | user_moderates_community |
| -------------- | ------------------------ |
| **Chaves** | {id_user, id_commmunity} |
| **Dependências Funcionais** | *nenhuma* |
| **Forma Normal** | BCNF |

Tabela 12 - Validação da Relação *user_moderates_community*

| **Tabela R10** | reputation |
| -------------- | ---------- |
| **Chaves** | {id_user, id_community} |
| **Dependências Funcionais** |
| FD1001 | {id_user, id_community} → {rating, expert} |
| **Forma Normal** | BCNF |

Tabela 13 - Validação da Relação *reputation*

| **Tabela R11** | question |
| -------------- | -------- |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD1101 | {id} → {content, date, file, last_edited, title, id_user, id_community} |
| **Forma Normal** | BCNF |

Tabela 14 - Validação da Relação *question*

| **Tabela R12** | user_follows_question |
| -------------- | --------------------- |
| **Chaves** | {id_user, id_question} |
| **Dependências Funcionais** | *nenhuma* |
| **Forma Normal** | BCNF |

Tabela 15 - Validação da Relação *user_follows_question*

| **Tabela R13** | question_tags |
| -------------- | ------------- |
| **Chaves** | {id_question, id_tag} |
| **Dependências Funcionais** | *nenhuma* |
| **Forma Normal** | BCNF |

Tabela 16 - Validação da Relação *question_tags*

| **Tabela R14** | question_vote |
| -------------- | ------------- |
| **Chaves** | {id_question, id_user} |
| **Dependências Funcionais** |
| FD1401 | {id_question, id_user} → {like} |
| **Forma Normal** | BCNF |

Tabela 17 - Validação da Relação *question_vote*

| **Tabela R15** | question_comment |
| -------------- | ---------------- |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD1501 | {id} → {content, date, last_edited, id_user, id_question} |
| **Forma Normal** | BCNF |

Tabela 18 - Validação da Relação *question_comment*

| **Tabela R16** | answer |
| -------------- | ------ |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD1601 | {id} → {content, date, file, last_edited, correct, id_user, id_question} |
| **Forma Normal** | BCNF |

Tabela 19 - Validação da Relação *answer*

| **Tabela R17** | answer_vote |
| -------------- | ----------- |
| **Chaves** | {id_answer, id_user} |
| **Dependências Funcionais** |
| FD1701 | {id_answer, id_user} → {like} |
| **Forma Normal** | BCNF |

Tabela 20 - Validação da Relação *answer_vote*

| **Tabela R18** | answer_comment |
| -------------- | -------------- |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD1801 | {id} → {content, date, last_edited, id_user, id_answer} |
| **Forma Normal** | BCNF |

Tabela 21 - Validação da Relação *answer_comment*


Não são necessárias quaisquer alterações para converter o esquema relacional para a Forma Normal da Boyce-Codd (BCNF), tendo em conta que todas as relações já se encontram na Forma Normal de Boyce-Codd. 

É possível comprovar este facto através da análise do conjunto de dependências funcionais associado a cada relação, dado que os atributos do lado esquerdo de cada dependência funcional de uma relação são sempre uma superchave dessa relação.
Aliás, em cada relação, o atributo iniciado por *id* é a chave primária da relação e os restantes atributos que aparecem em lados esquerdos das dependências funcionais dessa relação são chaves candidatas da relação, pois determinam funcionalmente todos os atributos da relação.


---


## A6: Índices, Gatilhos, Transações e População da Base de Dados Indexes

Este artefacto engloba a carga de trabalho da base de dados, a especificação dos índices, dos gatilhos, das transações e das funções definidas pelo utilizador, bem como os *scripts* para criação da base de dados e para o seu povoamento. Deste modo, o documento contém tudo aquilo que é necessário para a implementação da base de dados.


### 1. Carga de Trabalho da Base de Dados

O esquema físico da base de dados inclui - na tabela abaixo - uma análise da carga estimada do sistema, nomeadamente através de uma previsão do número aproximado de linhas em cada tabela e do seu crescimento esperado, sendo essencial compreender a ordem de grandeza e o crescimento das relações de modo a desenvolver uma base de dados sólida e eficiente.

| **Referência da Relação** | **Nome da Relação** | **Ordem de Magnitude** | **Crescimento Estimado (por dia)** |
| ------------------------- | ------------------- | ---------------------- | ---------------------------------- |
| R01 | user | 10 k | 10 |
| R02 | notification | 100 k | 100 |
| R03 | badge | 10 | 1 |
| R04 | user_earns_badge | 10 k | 10 |
| R05 | tag | 1 k | 1 |
| R06 | user_follows_tag | 1 k | 10 |
| R07 | community | 100 | 1 |
| R08 | user_follows_community | 10 k | 10 |
| R09 | user_moderates_community | 100 | 1 |
| R10 | reputation | 10 k | 10 |
| R11 | question | 10 k | 10 |
| R12 | user_follows_question | 1 k | 10 |
| R13 | question_tags | 10 k | 10 |
| R14 | question_vote | 100 k | 100 |
| R15 | question_comment | 1 k | 10 |
| R16 | answer | 10 k | 10 |
| R17 | answer_vote | 100 k | 100 |
| R18 | answer_comment | 1 k | 10 |

Tabela 22 - Carga de Trabalho da Base de Dados


### 2. Índices Propostos

#### 2.1. Índices de Desempenho
 
Nas três tabelas abaixo propõem-se os índices a implementar de maneira a melhorar o desempenho das consultas à base de dados.

| **Índice** | IDX01 |
| ---------- | ----- |
| **Relação** | notification |
| **Atributo** | id_user |
| **Tipo** | Hash |
| **Cardinalidade** | Média |
| **Agrupamento** | Não |
| **Justificação** | A tabela *notification* é muito grande (estima-se que tenha muitas entradas) e prevê-se que seja atualizada frequentemente, pelo que a importância da existência de um índice é acrescida. Como as consultas são feitas, em princípio, para um dado utilizador e a ordenação necessária é apenas por data (dentro do mesmo utilizador), o índice mais relevante é do tipo hash. Não se propõe agrupamento da tabela devido à estimativa elevada para a sua frequência de crescimento. |
| **Código SQL** | |
```sql
CREATE INDEX user_notification ON notification USING hash (id_user);
```

Tabela 22 - Índice para a Relação *notification*

| **Índice** | IDX02 |
| ---------- | ----- |
| **Relação** | question |
| **Atributo** | id_community |
| **Tipo** | B-tree |
| **Cardinalidade** | Média |
| **Agrupamento** | Sim |
| **Justificação** | A tabela *question* será relativamente grande, mas a frequência de atualização deve ser baixa. A par disto, as operações de pesquisa e ordenação poderão ser bastante frequentes. Como tal, um índice do tipo b-tree afigura-se o mais apropriado, sobre o atributo *id_community* porque, numa porção significativa dos casos, as consultas são efetuadas a perguntas inseridas numa comunidade. Também por este motivo - e tendo em conta que não se estima que a tabela seja atualizada muito frequentemente (e a cardinalidade é média) -, propõe-se o agrupamento das entradas da mesma. |
| **Código SQL** | |
```sql
CREATE INDEX community_question ON question USING btree (id_community);
CLUSTER question USING community_question;
```

Tabela 23 - Índice para a Relação *question*

| **Índice** | IDX03 |
| ---------- | ----- |
| **Relação** | answer |
| **Atributo** | id_question |
| **Tipo** | Hash |
| **Cardinalidade** | Média |
| **Agrupamento** | Não |
| **Justificação** | Estima-se que tabela *answer* tenha muitas entradas, mas que não cresça com tão grande frequência. Na maior parte dos casos (excluindo operações de pesquisa sobre respostas), as consultas a esta tabela serão feitas para uma dada pergunta. Deste modo, as comparações efetuadas nas interrogações à base de dados são apenas de igualdade e não existe necessidade de ordenação sobre o atributo comparado, pelo que um índice do tipo hash é o mais adequado. Não é proposto agrupamento dos dados visto que não funciona com índices deste tipo. |
| **Código SQL** | |
```sql
CREATE INDEX question_answer ON answer USING hash (id_question);
```

Tabela 24 - Índice para a Relação *question_vote*

Note-se que, dadas as semelhanças entre as relações *question* e *answer*, bem como *question_vote* e *answer_vote*, os índices a implementar para as primeiras tabelas também poderiam ser implementados para as segundas (com as devidas adaptações, mas pelos mesmos motivos).

#### 2.2. Índices para *Full-Text Search*

O sistema a desenvolver deve permitir *full-text search*, pelo que é necessário definir os índices e especificar os campos dos documentos sobre os quais eles operam. Tais definições/especificações e respetivas configurações necessárias encontram-se nas tabelas abaixo.

| **Índice** | IDX11 |
| ---------- | ----- |
| **Relação** | question |
| **Atributos** | title, content |
| **Tipo** | GIN |
| **Agrupamento** | Não |
| **Justificação** | Este índice é necessário para efetuar *full-text search* sobre perguntas, incluindo o seu título e conteúdo. O tipo do índice é GIN porque, pese embora estes campos sejam editáveis, não é esperado que sejam atualizados frequentemente - é mais importante uma pesquisa rápida do que uma atualização rápida. |
| **Código SQL** | |
```sql
-- Adicionar à tabela question uma coluna para armazenar os ts_vectors computados
ALTER TABLE question
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE FUNCTION question_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = (
            setweight(to_tsvector('english', NEW.title), 'A') ||
            setweight(to_tsvector('english', NEW.content), 'B')
        );
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF (NEW.title <> OLD.title OR NEW.content <> OLD.content) THEN
            NEW.tsvectors = (
                setweight(to_tsvector('english', NEW.title), 'A') ||
                setweight(to_tsvector('english', NEW.content), 'B')
            );
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER question_search_update
    BEFORE INSERT OR UPDATE ON question
    FOR EACH ROW
    EXECUTE PROCEDURE question_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX question_search_idx ON question USING GIN (tsvectors);
```

Tabela 25 - Índice para *Full-Text Search* sobre a Relação *question*

| **Índice** | IDX12 |
| ---------- | ----- |
| **Relação** | answer |
| **Atributo** | content |
| **Tipo** | GIN |
| **Agrupamento** | Não |
| **Justificação** | Este índice é necessário para efetuar *full-text search* sobre respostas, nomeadamente sobre o seu conteúdo. O tipo do índice é GIN porque, pese embora as respostas sejam editáveis, não é esperado que essa edição seja frequente - é mais importante uma pesquisa rápida do que uma atualização rápida. |
| **Código SQL** | |
```sql
-- Adicionar à tabela answer uma coluna para armazenar os ts_vectors computados
ALTER TABLE answer
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE FUNCTION answer_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER answer_search_update
    BEFORE INSERT OR UPDATE ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE answer_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX answer_search_idx ON answer USING GIN (tsvectors);
```

Tabela 26 - Índice para *Full-Text Search* sobre a Relação *answer*

| **Índice** | IDX13 |
| ---------- | ----- |
| **Relação** | question_comment |
| **Atributo** | content |
| **Tipo** | GIN |
| **Agrupamento** | Não |
| **Justificação** | Este índice é necessário para efetuar *full-text search* sobre comentários em perguntas, nomeadamente sobre o seu conteúdo. O tipo do índice é GIN porque, pese embora os comentários sejam editáveis, não é esperado que essa edição seja frequente - é mais importante uma pesquisa rápida do que uma atualização rápida. |
| **Código SQL** | |
```sql
-- Adicionar à tabela question_comment uma coluna para armazenar os ts_vectors computados
ALTER TABLE question_comment
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE FUNCTION question_comment_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER question_comment_search_update
    BEFORE INSERT OR UPDATE ON question_comment
    FOR EACH ROW
    EXECUTE PROCEDURE question_comment_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX question_comment_search_idx ON question_comment USING GIN (tsvectors);
```

Tabela 27 - Índice para *Full-Text Search* sobre a Relação *question_comment*

| **Índice** | IDX14 |
| ---------- | ----- |
| **Relação** | answer_comment |
| **Atributo** | content |
| **Tipo** | GIN |
| **Agrupamento** | Não |
| **Justificação** | Este índice é necessário para efetuar *full-text search* sobre comentários em respostas, nomeadamente sobre o seu conteúdo. O tipo do índice é GIN porque, pese embora os comentários sejam editáveis, não é esperado que essa edição seja frequente - é mais importante uma pesquisa rápida do que uma atualização rápida. |
| **Código SQL** | |
```sql
-- Adicionar à tabela answer_comment uma coluna para armazenar os ts_vectors computados
ALTER TABLE answer_comment
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE FUNCTION answer_comment_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER answer_comment_search_update
    BEFORE INSERT OR UPDATE ON answer_comment
    FOR EACH ROW
    EXECUTE PROCEDURE answer_comment_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX answer_comment_search_idx ON answer_comment USING GIN (tsvectors);
```

Tabela 28 - Índice para *Full-Text Search* sobre a Relação *answer_comment*

Note-se que as funções ```answer_search_update()```, ```question_comment_search_update()``` e ```answer_comment_search_update()```, presentes nos últimos três índices (IDX12, IDX13 e IDX14) - têm o corpo comum, isto é, são constituídas pelas mesmas instruções, dado que uma coluna de nome *content* está presente nas três relações (*answer*, *question_comment* e *answer_comment*, respetivamente) sobre as quais efetuar *full-text search*. Por este motivo, no *script* SQL de criação da base de dados, só se definirá a função uma vez - com o nome ```content_search_update()``` - e os gatilhos serão adaptados para chamar a função apropriada. Aqui, a função foi apresentada de forma triplicada para simplicidade na leitura/análise de cada bloco de código SQL.


### 3. Gatilhos

Os gatilhos desempenham um papel crucial na automatização de processos e na manutenção da integridade dos dados em sistemas de gestão de base de dados, garantindo a execução de determinadas ações em resposta a eventos específicos.

| **Trigger** | TRIGGER01 |
| ----------- | --------- |
| **Descrição** | Nenhum utilizador autenticado (*User*) pode votar (*Vote*) numa pergunta (*Question*) da qual é autor - **BR01** |
| **Código SQL** | |
```sql
CREATE FUNCTION prevent_self_vote_on_question() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.id_user = (SELECT id_user FROM question WHERE id = NEW.id_question) THEN
        RAISE EXCEPTION 'Cannot vote on your own question';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER prevent_self_vote_on_question
    BEFORE INSERT OR UPDATE ON question_vote
    FOR EACH ROW
    EXECUTE PROCEDURE prevent_self_vote_on_question();
```

Tabela 29 - Gatilho para impedir voto do autor nas perguntas

| **Trigger** | TRIGGER02 |
| ----------- | --------- |
| **Descrição** | Nenhum utilizador autenticado (*User*) pode votar (*Vote*) numa resposta (*Answer*) da qual é autor - **BR01** |
| **Código SQL** | |
```sql
CREATE FUNCTION prevent_self_vote_on_answer() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.id_user = (SELECT id_user FROM answer WHERE id = NEW.id_answer) THEN
        RAISE EXCEPTION 'Cannot vote on your own answer';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER prevent_self_vote_on_answer
    BEFORE INSERT OR UPDATE ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE prevent_self_vote_on_answer();
```

Tabela 30 - Gatilho para impedir voto do autor nas respostas

| **Trigger** | TRIGGER03 |
| ----------- | --------- |
| **Descrição** | Se um utilizador autenticado (*User*) for apagado, o conteúdo do qual é autor mantém-se na base de dados com um autor anónimo - **BR02** |
| **Código SQL** | |
```sql
CREATE FUNCTION update_content_on_user_deletion() RETURNS TRIGGER AS
$BODY$
BEGIN
    UPDATE users SET username = CONCAT('anonymous', OLD.id), email = CONCAT('anonymous', OLD.id) WHERE id = OLD.id;
    RETURN NULL;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER update_content_on_user_deletion
    BEFORE DELETE ON users
    FOR EACH ROW
    EXECUTE PROCEDURE update_content_on_user_deletion();
```

Tabela 31 - Gatilho para anonimizar utilizadores apagados

| **Trigger** | TRIGGER04 |
| ----------- | --------- |
| **Descrição** | O *rating* de um utilizador autenticado (*User*) numa comunidade (*Community*) é calculado de acordo com a fórmula 1000 x likes/(likes + dislikes) nas suas respostas (*Answer*) dentro dessa comunidade - **BR03** |
| **Código SQL** | |
```sql	
CREATE FUNCTION calculate_user_rating() RETURNS TRIGGER AS
$BODY$
DECLARE
    total_likes INTEGER;
    total_dislikes INTEGER;
    id_author INTEGER;
    id_c INTEGER;
BEGIN
    SELECT id_user INTO id_author
    FROM answer
    WHERE answer.id = NEW.id_answer;

    SELECT id_community INTO id_c
    FROM answer JOIN question ON answer.id_question = question.id
    WHERE answer.id = NEW.id_answer;

    SELECT COUNT(*) INTO total_likes
    FROM question JOIN answer ON question.id = answer.id_question JOIN answer_vote ON answer_vote.id_answer = answer.id 
    WHERE answer.id_user = id_author AND question.id_community = id_c AND answer_vote.likes = TRUE;

    SELECT COUNT(*) INTO total_dislikes
    FROM question JOIN answer ON question.id = answer.id_question JOIN answer_vote ON answer_vote.id_answer = answer.id 
    WHERE answer.id_user = id_author AND question.id_community = id_c AND answer_vote.likes = FALSE;
    IF total_likes + total_dislikes = 0 THEN
        RETURN NEW;
    END IF;

    IF EXISTS (SELECT id_user FROM reputation WHERE id_user = id_author) THEN
        UPDATE reputation
        SET rating = 1000 * total_likes / (total_likes + total_dislikes)
        WHERE id_user = id_author AND id_community = id_c;
    ELSE
        INSERT INTO reputation (id_user, id_community, rating)
        VALUES (id_author, id_c, 1000 * total_likes / (total_likes + total_dislikes));
    END IF;

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER calculate_user_rating
    AFTER INSERT OR UPDATE ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE calculate_user_rating();
```

Tabela 32 - Gatilho para calcular *rating*

| **Trigger** | TRIGGER05 |
| ----------- | --------- |
| **Descrição** | Um utilizador autenticado (*User*) é *expert* de uma comunidade (*Community*) se e só se tiver todos os emblemas (*Badge*) possíveis e um *rating* superior a 800 - **BR04** |
| **Código SQL** | |
```sql
CREATE FUNCTION check_expert_status() RETURNS TRIGGER AS
$BODY$
DECLARE
    total_badges INTEGER;
    user_badges INTEGER;
BEGIN
    SELECT COUNT(*) INTO total_badges FROM badge;
    SELECT COUNT(*) INTO user_badges FROM user_earns_badge WHERE id_user = NEW.id_user;

    IF total_badges = user_badges AND NEW.rating > 800 THEN
        SET NEW.expert = TRUE;
    ELSE
        SET NEW.expert = FALSE;
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_expert_status
    AFTER INSERT OR UPDATE ON reputation
    FOR EACH ROW
    EXECUTE PROCEDURE check_expert_status();
```

Tabela 33 - Gatilho para verificar título de *expert*

| **Trigger** | TRIGGER06 |
| ----------- | --------- |
| **Descrição** | Os ficheiros (*file*) das perguntas (*Question*) devem ter uma das seguintes extensões (ou seja, terminar em) *jpg*, *jpeg*, *png*, *txt*, *pdf*, *doc* - **BR05** |
| **Código SQL** | |
```sql	
CREATE FUNCTION check_file_extension() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.file NOT LIKE '%.jpg' AND 
       NEW.file NOT LIKE '%.jpeg' AND 
       NEW.file NOT LIKE '%.png' AND 
       NEW.file NOT LIKE '%.txt' AND 
       NEW.file NOT LIKE '%.pdf' AND 
       NEW.file NOT LIKE '%.doc' THEN
       RAISE EXCEPTION 'Invalid file extension for %. Only jpg, jpeg, png, txt, pdf, doc are allowed.', NEW.file;
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_file_extension_on_question
    BEFORE INSERT OR UPDATE ON question
    FOR EACH ROW
    EXECUTE PROCEDURE check_file_extension();
```

Tabela 34 - Gatilho para verificar extensão do ficheiro nas perguntas

| **Trigger** | TRIGGER07 |
| ----------- | --------- |
| **Descrição** | Os ficheiros (*file*) das respostas (*Answer*) devem ter uma das seguintes extensões (ou seja, terminar em) *jpg*, *jpeg*, *png*, *txt*, *pdf*, *doc* - **BR05** |
| **Código SQL** | |
```sql
CREATE FUNCTION check_file_extension() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.file NOT LIKE '%.jpg' AND 
       NEW.file NOT LIKE '%.jpeg' AND 
       NEW.file NOT LIKE '%.png' AND 
       NEW.file NOT LIKE '%.txt' AND 
       NEW.file NOT LIKE '%.pdf' AND 
       NEW.file NOT LIKE '%.doc' THEN
       RAISE EXCEPTION 'Invalid file extension for %. Only jpg, jpeg, png, txt, pdf, doc are allowed.', NEW.file;
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_file_extension_on_answer
    BEFORE INSERT OR UPDATE ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE check_file_extension();
```

Tabela 35 - Gatilho para verificar extensão do ficheiro nas respostas

| **Trigger** | TRIGGER08 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) só pode votar (*Vote*) em cada pergunta (*Question*) uma vez - **BR06** |
| **Código SQL** | |
```sql
CREATE FUNCTION check_question_vote() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF EXISTS (SELECT id_user FROM question_vote WHERE id_question = NEW.id_question AND id_user = NEW.id_user) THEN
        RAISE EXCEPTION 'Each user can only vote once on each question.';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_question_vote
    BEFORE INSERT OR UPDATE ON question_vote
    FOR EACH ROW
    EXECUTE PROCEDURE check_question_vote();
```

Tabela 36 - Gatilho para verificar voto único nas perguntas

| **Trigger** | TRIGGER09 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) só pode votar (*Vote*) em cada resposta uma vez - **BR06** |
| **Código SQL** | |
```sql
CREATE FUNCTION check_answer_vote() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF EXISTS (SELECT id_user FROM answer_vote WHERE id_answer = NEW.id_answer AND id_user = NEW.id_user) THEN
        RAISE EXCEPTION 'Each user can only vote once on each answer.';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_answer_vote
    BEFORE INSERT ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE check_answer_vote();
```

Tabela 37 - Gatilho para verificar voto único nas respostas

| **Trigger** | TRIGGER10 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber notificações (*Notification*) de respostas (*Answer*) às próprias perguntas (*Question*) |
| **Código SQL** | |
```sql	
CREATE FUNCTION new_answer_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT id_user, title INTO author, q_title
    FROM question
    WHERE id = NEW.id_question;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new answer to your question: ', q_title, '!'), CURRENT_DATE, FALSE, author);
    
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_answer_notification
    AFTER INSERT ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE new_answer_notification();
```

Tabela 38 - Gatilho para enviar notificações de respostas às próprias perguntas

| **Trigger** | TRIGGER11 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber notificações (*Notification*) de votos (*Vote*) nas próprias perguntas (*Question*) |
| **Código SQL** | |
```sql
CREATE FUNCTION new_question_vote_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT id_user, title INTO author, q_title
    FROM question
    WHERE id = NEW.id_question;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new vote on your question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_question_vote_notification
    AFTER INSERT ON question_vote
    FOR EACH ROW
    EXECUTE PROCEDURE new_question_vote_notification();
```

Tabela 39 - Gatilho para enviar notificações de votos nas próprias perguntas

| **Trigger** | TRIGGER12 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber notificações (*Notification*) de votos (*Vote*) nas próprias respostas (*Answer*) |
| **Código SQL** | |
```sql
CREATE FUNCTION new_answer_vote_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT answer.id_user, title INTO author, q_title
    FROM answer JOIN question ON answer.id_question = question.id
    WHERE answer.id = NEW.id_answer;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new vote on your answer to the question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_answer_vote_notification
    AFTER INSERT ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE new_answer_vote_notification();
```

Tabela 40 - Gatilho para enviar notificações de votos nas próprias respostas

| **Trigger** | TRIGGER13 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber notificações (*Notification*) de comentários (*Comment*) às próprias perguntas (*Question*) |
| **Código SQL** | |
```sql
CREATE FUNCTION new_question_comment_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT id_user, title INTO author, q_title
    FROM question
    WHERE id = NEW.id_question;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new comment on your question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_question_comment_notification
    AFTER INSERT ON question_comment
    FOR EACH ROW
    EXECUTE PROCEDURE new_question_comment_notification();
```

Tabela 41 - Gatilho para enviar notificações de comentários às próprias perguntas

| **Trigger** | TRIGGER14 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber notificações (*Notification*) de comentários (*Comment*) às próprias respostas (*Answer*) |
| **Código SQL** | |
```sql
CREATE FUNCTION new_answer_comment_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT answer.id_user, title INTO author, q_title
    FROM answer JOIN question ON answer.id_question = question.id
    WHERE answer.id = NEW.id_answer;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new comment on your answer to the question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_answer_comment_notification
    AFTER INSERT ON answer_comment
    FOR EACH ROW
    EXECUTE PROCEDURE new_answer_comment_notification();
```

Tabela 42 - Gatilho para enviar notificações de comentários às próprias respostas

| **Trigger** | TRIGGER15 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber notificações (*Notification*) de atribuição de emblemas (*Badge*) |
| **Código SQL** | |
```sql
CREATE FUNCTION new_badge_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    b_name VARCHAR(255);
BEGIN
    SELECT name INTO b_name
    FROM badge
    WHERE id = NEW.id_badge;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new badge: ', b_name, '!'), CURRENT_DATE, FALSE, NEW.id_user);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_badge_notification
    AFTER INSERT ON user_earns_badge
    FOR EACH ROW
    EXECUTE PROCEDURE new_badge_notification();
```

Tabela 43 - Gatilho para enviar notificações de atribuição de emblemas

| **Trigger** | TRIGGER16 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber o emblema (*Badge*) da primeira pergunta (*Question*) realizada |
| **Código SQL** | |
```sql
CREATE FUNCTION award_badge_on_first_question() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM question WHERE id_user = NEW.id_user) = 1 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 1);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_question
    AFTER INSERT ON question
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_question();
```

Tabela 44 - Gatilho para atribuir o emblema da primeira pergunta

| **Trigger** | TRIGGER17 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber o emblema (*Badge*) da primeira resposta (*Answer*) realizada |
| **Código SQL** | |
```sql
CREATE FUNCTION award_badge_on_first_answer() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM answer WHERE id_user = NEW.id_user) = 1 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 2);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_answer
    AFTER INSERT ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_answer();
```

Tabela 45 - Gatilho para atribuir o emblema da primeira resposta

| **Trigger** | TRIGGER18 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber o emblema (*Badge*) do primeiro comentário (*Comment*) realizado numa pergunta (*Question*) |
| **Código SQL** | |
```sql
CREATE FUNCTION award_badge_on_first_comment_question() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM question_comment WHERE id_user = NEW.id_user) = 1 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 3);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_comment_question
    AFTER INSERT ON question_comment
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_comment_question();
```

Tabela 46 - Gatilho para atribuir o emblema do primeiro comentário numa pergunta

| **Trigger** | TRIGGER19 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber o emblema (*Badge*) do primeiro comentário (*Comment*) realizado numa resposta (*Answer*) |
| **Código SQL** | |
```sql
CREATE FUNCTION award_badge_on_first_comment_answer() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM answer_comment WHERE id_user = NEW.id_user) = 1 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 4);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_comment_answer
    AFTER INSERT ON answer_comment
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_comment_answer();
```

Tabela 47 - Gatilho para atribuir o emblema do primeiro comentário numa resposta

| **Trigger** | TRIGGER20 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber o emblema (*Badge*) das primeira 100 perguntas (*Question*) realizadas |
| **Código SQL** | |
```sql
CREATE FUNCTION award_badge_on_first_100_question() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM question WHERE id_user = NEW.id_user) = 100 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 5);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_100_question
    AFTER INSERT ON question
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_100_question();
```

Tabela 48 - Gatilho para atribuir o emblema das primeiras 100 perguntas

| **Trigger** | TRIGGER21 |
| ----------- | --------- |
| **Descrição** | Cada utilizador autenticado (*User*) deve receber o emblema (*Badge*) das primeiras 100 respostas (*Answer*) realizadas |
| **Código SQL** | |
```sql
CREATE FUNCTION award_badge_on_first_100_answer() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM answer WHERE id_user = NEW.id_user) = 100 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 6);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_100_answer
    AFTER INSERT ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_100_answer();
```

Tabela 49 - Gatilho para atribuir o emblema das primeiras 100 respostas

Note-se que a função ```check_file_extension()``` é comum aos *triggers* TRIGGER06 e TRIGGER07, dado que uma coluna de nome ```file``` está presente nas duas relações (*question* e *answer*, respetivamente) sobre as quais o *trigger* deve atuar. Por este motivo, no *script* SQL de criação da base de dados, só se definirá a função uma vez. Aqui, a função foi apresentada de forma duplicada para simplicidade na leitura/análise de cada bloco de código SQL.


### 4. Transações

As transações desempenham um papel crucial na garantia da integridade e da consistência dos dados, permitindo que múltiplos utilizadores acedam e modifiquem as mesmas informações na base de dados de forma segura e coordenada.

| **Transação** | TRAN01 |
| ------------- | ------ |
| **Descrição** | Obter os comentários, as respostas e os comentários das respostas a uma pergunta |
| **Justificação** | No meio da transação, poderiam ser publicados novos comentários, respostas ou comentários às respostas de uma pergunta, o que resultaria num *Phantom Read* - esta transação resolve esse problema (é READ ONLY porque só faz operações de seleção) |
| **Nível de Isolamento** | SERIALIZABLE READ ONLY |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL SERIALIZABLE READ ONLY;

-- Obter a pergunta
SELECT title, content, date, file, last_edited, username, image
FROM question JOIN users ON question.id_user = users.id
WHERE question.id = $id_question;

-- Obter os comentários da pergunta
SELECT content, date, last_edited, username
FROM question JOIN question_comment ON question.id = question_comment.id_question JOIN users ON question_comment.id_user = users.id
WHERE question.id = $id_question;

-- Obter as respostas à pergunta
SELECT content, date, file, last_edited, username, image
FROM question JOIN answer ON question.id = answer.id_question JOIN users ON answer.id_user = users.id
WHERE question.id = $id_question;

-- Obter os comentários das respostas à pergunta
SELECT content, date, last_edited, username
FROM answer JOIN answer_comment ON answer.id = answer_comment.id_answer JOIN users ON answer_comment.id_user = users.id
WHERE answer.id = $id_answer;

END TRANSACTION;
```

Tabela 50 - Transação para obter os comentários, as respostas e os comentários das respostas a uma pergunta

| **Transação** | TRAN02 |
| ------------- | ------ |
| **Descrição** | Pesquisar perguntas por *tags* |
| **Justificação** | No meio da transação, poderiam ser criadas/removidas *tags* da base de dados, o que resultaria num *Phantom Read* - esta transação resolve esse problema (é READ ONLY porque só faz operações de seleção) |
| **Nível de Isolamento** | SERIALIZABLE READ ONLY |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL SERIALIZABLE READ ONLY;

-- Obter as tags
SELECT name
FROM tag;

-- Pesquisar perguntas por tags
SELECT title, content, date
FROM question JOIN question_tags ON id = id_question
WHERE id_tag = $id_tag;

END TRANSACTION;
```

Tabela 51 - Transação para pesquisar perguntas por *tags*

| **Transação** | TRAN03 |
| ------------- | ------ |
| **Descrição** | Ver fóruns das comunidades |
| **Justificação** | No meio da transação, poderiam ser criadas/removidas comunidades da base de dados, o que resultaria num *Phantom Read* - esta transação resolve esse problema (é READ ONLY porque só faz operações de seleção) |
| **Nível de Isolamento** | SERIALIZABLE READ ONLY |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL SERIALIZABLE READ ONLY;

-- Obter as comunidades
SELECT name
FROM community;

-- Ver o fórum da comunidade
SELECT title, content, date, username, image
FROM question JOIN community ON question.id_community = community.id JOIN users ON question.id_user = users.id
WHERE question.id_community = $id_community;

END TRANSACTION;
```

Tabela 52 - Transação para ver fóruns das comunidades

| **Transação** | TRAN04 |
| ------------- | ------ |
| **Descrição** | Publicar perguntas com *tags* |
| **Justificação** | No meio da transação, poderia ser inserida outra questão na base de dados, atualizando inesperadamente o valor de *question_id_seq* e armazenando informação inconsistente na base de dados - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Inserir uma pergunta
INSERT INTO question (content, date, file, last_edited, title, id_user, id_community)
VALUES ($content, $date, $file, $last_edited, $title, $id_user, $id_community);

-- Associar a tag à pergunta
INSERT INTO question_tags (id_question, id_tag)
VALUES (currval('question_id_seq'), $id_tag);

END TRANSACTION;
```

Tabela 53 - Transação para publicar perguntas com *tags*

| **Transação** | TRAN05 |
| ------------- | ------ |
| **Descrição** | Votar numa pergunta |
| **Justificação** | No meio da transação, poderia ser removida/editada a pergunta da base de dados, o que resultaria num *Non-Repeatable Read* - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Obter a pergunta
SELECT title, content, date, file, last_edited, username, image
FROM question JOIN users ON question.id_user = users.id
WHERE question.id = $id_question;

-- Votar na pergunta
INSERT INTO question_vote (id_question, id_user, likes)
VALUES ($id_question, $id_user, $likes);

END TRANSACTION;
```

Tabela 54 - Transação para votar numa pergunta

| **Transação** | TRAN06 |
| ------------- | ------ |
| **Descrição** | Votar numa resposta |
| **Justificação** | No meio da transação, poderia ser removida/editada a resposta da base de dados, o que resultaria num *Non-Repeatable Read* - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Obter a resposta
SELECT title, content, date, file, last_edited, username, image
FROM answer JOIN users ON question.id_user = users.id
WHERE answer.id = $id_answer;

-- Votar na resposta
INSERT INTO answer_vote (id_answer, id_user, likes)
VALUES ($id_answer, $id_user, $likes);

END TRANSACTION;
```

Tabela 55 - Transação para votar numa resposta

| **Transação** | TRAN07 |
| ------------- | ------ |
| **Descrição** | Comentar numa pergunta |
| **Justificação** | No meio da transação, poderia ser removida/editada a pergunta da base de dados, o que resultaria num *Non-Repeatable Read* - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Obter a pergunta
SELECT title, content, date, file, last_edited, username, image
FROM question JOIN users ON question.id_user = users.id
WHERE question.id = $id_question;

-- Comentar na pergunta
INSERT INTO question_comment (content, id_question, id_user)
VALUES ($content, $id_question, $id_user);

END TRANSACTION;
```

Tabela 56 - Transação para comentar numa pergunta

| **Transação** | TRAN08 |
| ------------- | ------ |
| **Descrição** | Comentar numa resposta |
| **Justificação** | No meio da transação, poderia ser removida/editada a resposta da base de dados, o que resultaria num *Non-Repeatable Read* - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Obter a resposta
SELECT title, content, date, file, last_edited, username, image
FROM answer JOIN users ON question.id_user = users.id
WHERE answer.id = $id_answer;

-- Comentar na resposta
INSERT INTO answer_comment (content, id_answer, id_user)
VALUES ($content, $id_answer, $id_user);

END TRANSACTION;
```

Tabela 57 - Transação para comentar numa resposta

| **Transação** | TRAN09 |
| ------------- | ------ |
| **Descrição** | Mostrar interesse numa pergunta |
| **Justificação** | No meio da transação, poderia ser removida/editada a pergunta da base de dados, o que resultaria num *Non-Repeatable Read* - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Obter a pergunta
SELECT title, content, date, file, last_edited, username, image
FROM question JOIN users ON question.id_user = users.id
WHERE question.id = $id_question;

-- Mostrar interesse na pergunta
INSERT INTO user_follows_question (id_user, id_question)
VALUES ($id_user, $id_question);

END TRANSACTION;
```

Tabela 58 - Transação para mostrar interesse numa pergunta

| **Transação** | TRAN10 |
| ------------- | ------ |
| **Descrição** | Mostrar interesse numa *tag* |
| **Justificação** | No meio da transação, poderia ser removida/editada a *tag* da base de dados, o que resultaria num *Non-Repeatable Read* - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Obter as tags
SELECT name
FROM tag;

-- Mostrar interesse na tag
INSERT INTO user_follows_tag (id_user, id_tag)
VALUES ($id_user, $id_tag);

END TRANSACTION;
```

Tabela 59 - Transação para mostrar interesse numa *tag*

| **Transação** | TRAN11 |
| ------------- | ------ |
| **Descrição** | Receber uma notificação de uma resposta |
| **Justificação** | No meio da transação, poderia ser publicada outra resposta que levasse a uma atualização na tabela de notificações, armazenando informação inconsistente na base de dados - esta transação resolve esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Publicar a resposta
INSERT INTO answer (content, file, id_user, id_question)
VALUES ($answer_content, $file, $id_user, $id_question);

-- Obter o autor da pergunta ($id_author)
SELECT id_user
FROM question
WHERE id = $id_question;

-- Receber a notificação
INSERT INTO notification (content, id_user)
VALUES ($notification_content, $id_author);

END TRANSACTION;
```

Tabela 60 - Transação para receber uma notificação de uma resposta

| **Transação** | TRAN12 |
| ------------- | ------ |
| **Descrição** | Receber uma notificação de um voto numa pergunta |
| **Justificação** | No meio da transação, poderia ser publicada outro voto numa pergunta que levasse a uma atualização na tabela de notificações, armazenando informação inconsistente na base de dados - esta transação resolve esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Publicar o voto
INSERT INTO question_vote (id_user, id_question, likes)
VALUES ($id_user, $id_question, $likes);

-- Obter o autor da pergunta ($id_author)
SELECT id_user
FROM question
WHERE id = $id_question;

-- Receber a notificação
INSERT INTO notification (content, id_user)
VALUES ($content, $id_author);

END TRANSACTION;
```

Tabela 61 - Transação para receber uma notificação de um voto numa pergunta

| **Transação** | TRAN13 |
| ------------- | ------ |
| **Descrição** | Receber uma notificação de um voto numa resposta |
| **Justificação** | No meio da transação, poderia ser publicada outro voto numa resposta que levasse a uma atualização na tabela de notificações, armazenando informação inconsistente na base de dados - esta transação resolve esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Publicar o voto
INSERT INTO answer_vote (id_user, id_answer, likes)
VALUES ($id_user, $id_answer, $likes);

-- Obter o autor da resposta ($id_author)
SELECT id_user
FROM answer
WHERE id = $id_answer;

-- Receber a notificação
INSERT INTO notification (content, id_user)
VALUES ($content, $id_author);

END TRANSACTION;
```

Tabela 62 - Transação para receber uma notificação de um voto numa resposta

| **Transação** | TRAN14 |
| ------------- | ------ |
| **Descrição** | Obter o número de notificações por ler e essas notificações |
| **Justificação** | No meio da transação, poderiam ser publicadas novas notificações (por ler), o que resultaria num *Phantom Read* - esta transação resolve esse problema (é READ ONLY porque só faz operações de seleção) |
| **Nível de Isolamento** | SERIALIZABLE READ ONLY |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL SERIALIZABLE READ ONLY;

-- Obter o número de notificações por ler
SELECT COUNT(*)
FROM notification
WHERE id_user = $id_user AND read = FALSE;

-- Obter as notificações por ler
SELECT content, date
FROM notification
WHERE id_user = $id_user AND read = FALSE;

END TRANSACTION;
```

Tabela 63 - Transação para obter o número de notificações por ler e essas notificações

| **Transação** | TRAN15 |
| ------------- | ------ |
| **Descrição** | Receber uma notificação de um emblema ganho |
| **Justificação** | No meio da transação, poderia ser ganho um emblema por outro utilizador, atualizando inesperadamente o valor de *user_earns_badge_id_user_seq* e armazenando informação inconsistente na base de dados - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Ganhar o emblema
INSERT INTO user_earns_badge (id_user, id_badge)
VALUES ($id_user, $id_badge);

-- Receber a notificação
INSERT INTO notification (content, id_user)
VALUES ($content, currval('user_earns_badge_id_user_seq'));

END TRANSACTION;
```

Tabela 64 - Transação para receber uma notificação de um emblema ganho

| **Transação** | TRAN16 |
| ------------- | ------ |
| **Descrição** | Receber uma notificação de um comentário numa pergunta |
| **Justificação** | No meio da transação, poderia ser publicado outro comentário numa pergunta que levasse a uma atualização na tabela de notificações, armazenando informação inconsistente na base de dados - esta transação resolve esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Publicar o comentário
INSERT INTO question_comment (content, id_user, id_question)
VALUES ($content, $id_user, $id_question);

-- Obter o autor da pergunta ($id_author)
SELECT id_user
FROM question
WHERE id = $id_question;

-- Receber a notificação
INSERT INTO notification (content, id_user)
VALUES ($content, $id_author);

END TRANSACTION;
```

Tabela 65 - Transação para receber uma notificação de um comentário numa pergunta

| **Transação** | TRAN17 |
| ------------- | ------ |
| **Descrição** | Receber uma notificação de um comentário numa resposta |
| **Justificação** | No meio da transação, poderia ser publicado outro comentário numa resposta que levasse a uma atualização na tabela de notificações, armazenando informação inconsistente na base de dados - esta transação resolve esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Publicar o comentário
INSERT INTO answer_comment (content, id_user, id_answer)
VALUES ($content, $id_user, $id_answer);

-- Obter o autor da resposta ($id_author)
SELECT id_user
FROM answer
WHERE id = $id_answer;

-- Receber a notificação
INSERT INTO notification (content, id_user)
VALUES ($content, $id_author);

END TRANSACTION;
```

Tabela 66 - Transação para receber uma notificação de um comentário numa resposta

| **Transação** | TRAN18 |
| ------------- | ------ |
| **Descrição** | Seguir uma comunidade |
| **Justificação** | No meio da transação, poderia ser removida/editada a comunidade da base de dados, o que resultaria num *Non-Repeatable Read* - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Obter as comunidades
SELECT name
FROM tag;

-- Seguir uma comunidade
INSERT INTO user_follows_community (id_user, id_community)
VALUES ($id_user, $id_community);

END TRANSACTION;
```

Tabela 67 - Transação para seguir uma comunidade

| **Transação** | TRAN19 |
| ------------- | ------ |
| **Descrição** | Editar *tags* de uma pergunta |
| **Justificação** | No meio da transação, poderiam ser removidas/editadas *tags* da base de dados, o que resultaria num *Non-Repeatable Read* - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Obter as tags
SELECT name
FROM tag;

-- Editar as tags de uma pergunta
UPDATE question_tags
SET id_tag = $id_tag
WHERE id_question = $id_question;

END TRANSACTION;
```

Tabela 68 - Transação para editar as *tags* de uma pergunta

| **Transação** | TRAN20 |
| ------------- | ------ |
| **Descrição** | Marcar uma resposta como correta |
| **Justificação** | No meio da transação, poderia ser removida/editada a pergunta da base de dados, o que resultaria num *Non-Repeatable Read* - esta transação soluciona esse problema |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** | |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Obter as respostas a uma pergunta
SELECT content, date, file, last_edited, username, image
FROM question JOIN answer ON question.id = answer.id_question JOIN users ON answer.id_user = users.id
WHERE question.id = $id_question;

-- Marcar uma resposta como correta
UPDATE answer
SET correct = TRUE
WHERE id_answer = $id_answer;

END TRANSACTION;
```

Tabela 69 - Transação para marcar uma resposta como correta


---


## Anexo A. Código SQL

Neste anexo incluem-se os *scripts* de criação e de povoamento da base de dados, bem como *links* para os respetivos ficheiros.


### A.1. Esquema da Base de Dados

O *script* de criação da base de dados presente abaixo inclui o código necessário para (re)construir a estrutura da base de dados.

O *script* também se encontra disponível em LINK.

```sql
CREATE SCHEMA IF NOT EXISTS lbaw23114;
SET search_path TO lbaw23114;


DROP TABLE IF EXISTS answer_comment CASCADE;
DROP TABLE IF EXISTS answer_vote CASCADE;
DROP TABLE IF EXISTS answer CASCADE;
DROP TABLE IF EXISTS question_comment CASCADE;
DROP TABLE IF EXISTS question_vote CASCADE;
DROP TABLE IF EXISTS question_tags CASCADE;
DROP TABLE IF EXISTS user_follows_question CASCADE;
DROP TABLE IF EXISTS question CASCADE;
DROP TABLE IF EXISTS reputation CASCADE;
DROP TABLE IF EXISTS user_moderates_community CASCADE;
DROP TABLE IF EXISTS user_follows_community CASCADE;
DROP TABLE IF EXISTS community CASCADE;
DROP TABLE IF EXISTS user_follows_tag CASCADE;
DROP TABLE IF EXISTS tag CASCADE;
DROP TABLE IF EXISTS user_earns_badge CASCADE;
DROP TABLE IF EXISTS badge CASCADE;
DROP TABLE IF EXISTS notification CASCADE;
DROP TABLE IF EXISTS users CASCADE;

DROP FUNCTION IF EXISTS content_search_update CASCADE;
DROP FUNCTION IF EXISTS question_search_update CASCADE;

DROP FUNCTION IF EXISTS award_badge_on_first_100_answer CASCADE;
DROP FUNCTION IF EXISTS award_badge_on_first_100_question CASCADE;
DROP FUNCTION IF EXISTS award_badge_on_first_comment_answer CASCADE;
DROP FUNCTION IF EXISTS award_badge_on_first_comment_question CASCADE;
DROP FUNCTION IF EXISTS award_badge_on_first_answer CASCADE;
DROP FUNCTION IF EXISTS award_badge_on_first_question CASCADE;
DROP FUNCTION IF EXISTS new_badge_notification CASCADE;
DROP FUNCTION IF EXISTS new_answer_comment_notification CASCADE;
DROP FUNCTION IF EXISTS new_question_comment_notification CASCADE;
DROP FUNCTION IF EXISTS new_answer_vote_notification CASCADE;
DROP FUNCTION IF EXISTS new_question_vote_notification CASCADE;
DROP FUNCTION IF EXISTS new_answer_notification CASCADE;
DROP FUNCTION IF EXISTS check_answer_vote CASCADE;
DROP FUNCTION IF EXISTS check_question_vote CASCADE;
DROP FUNCTION IF EXISTS check_file_extension CASCADE;
DROP FUNCTION IF EXISTS check_expert_status CASCADE;
DROP FUNCTION IF EXISTS calculate_user_rating CASCADE;
DROP FUNCTION IF EXISTS update_content_on_user_deletion CASCADE;
DROP FUNCTION IF EXISTS prevent_self_vote_on_answer CASCADE;
DROP FUNCTION IF EXISTS prevent_self_vote_on_question CASCADE;


-- Tabelas


CREATE TABLE users ( -- 'users' porque 'user' é uma palavra reservada em PostgreSQL
    id SERIAL PRIMARY KEY,
    username VARCHAR(255) NOT NULL CONSTRAINT user_username_uk UNIQUE,
    email VARCHAR(255) NOT NULL CONSTRAINT user_email_uk UNIQUE,
    password VARCHAR(255) NOT NULL,
    register_date DATE NOT NULL DEFAULT CURRENT_DATE,
    administrator BOOLEAN NOT NULL DEFAULT FALSE,
    blocked BOOLEAN NOT NULL DEFAULT FALSE,
    image VARCHAR(255)
);

CREATE TABLE notification (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL DEFAULT CURRENT_DATE,
    read BOOLEAN NOT NULL DEFAULT FALSE,
    id_user INTEGER NOT NULL REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE badge (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL CONSTRAINT badge_name_uk UNIQUE
);

CREATE TABLE user_earns_badge (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_badge INTEGER REFERENCES badge (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_badge)
);

CREATE TABLE tag (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL CONSTRAINT tag_name_uk UNIQUE
);

CREATE TABLE user_follows_tag (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_tag INTEGER REFERENCES tag (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_tag)
);

CREATE TABLE community (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL CONSTRAINT community_name_uk UNIQUE
);

CREATE TABLE user_follows_community (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_community INTEGER REFERENCES community (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_community)
);

CREATE TABLE user_moderates_community (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_community INTEGER REFERENCES community (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_community)
);

CREATE TABLE reputation (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_community INTEGER REFERENCES community (id) ON UPDATE CASCADE ON DELETE CASCADE,
    rating INTEGER NOT NULL DEFAULT 0,
    expert BOOLEAN NOT NULL DEFAULT FALSE,
    PRIMARY KEY (id_user, id_community),
    CONSTRAINT reputation_rating_ck CHECK (rating >= 0)
);

CREATE TABLE question (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL DEFAULT CURRENT_DATE,
    file VARCHAR(255),
    last_edited DATE,
    title TEXT NOT NULL,
    id_user INTEGER NOT NULL REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_community INTEGER NOT NULL REFERENCES community (id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT question_last_edited_ck CHECK (last_edited >= date)
);

CREATE TABLE user_follows_question (
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_question INTEGER REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_user, id_question)
);

CREATE TABLE question_tags (
    id_question INTEGER REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_tag INTEGER REFERENCES tag (id) ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (id_question, id_tag)
);

CREATE TABLE question_vote (
    id_question INTEGER REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    likes BOOLEAN NOT NULL, -- 'likes' porque 'like' é uma palavra reservada em PostgreSQL
    PRIMARY KEY (id_question, id_user)
);

CREATE TABLE question_comment (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL DEFAULT CURRENT_DATE,
    last_edited DATE,
    id_user INTEGER NOT NULL REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_question INTEGER NOT NULL REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT question_comment_last_edited_ck CHECK (last_edited >= date)
);

CREATE TABLE answer (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL DEFAULT CURRENT_DATE,
    file VARCHAR(255),
    last_edited DATE,
    correct BOOLEAN NOT NULL DEFAULT FALSE,
    id_user INTEGER NOT NULL REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_question INT NOT NULL REFERENCES question (id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT answer_last_edited_ck CHECK (last_edited >= date)
);

CREATE TABLE answer_vote (
    id_answer INTEGER REFERENCES answer (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    likes BOOLEAN NOT NULL, -- 'likes' porque 'like' é uma palavra reservada em PostgreSQL
    PRIMARY KEY (id_answer, id_user)
);

CREATE TABLE answer_comment (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    date DATE NOT NULL DEFAULT CURRENT_DATE,
    last_edited DATE,
    id_user INTEGER REFERENCES users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    id_answer INTEGER REFERENCES answer (id) ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT answer_comment_ck CHECK (last_edited >= date)
);


-- Índices de Desempenho


CREATE INDEX user_notification ON notification USING hash (id_user);

CREATE INDEX community_question ON question USING btree (id_community);
CLUSTER question USING community_question;

CREATE INDEX question_answer ON answer USING hash (id_question);


-- Índices para Full-Text Search


-- Adicionar à tabela question uma coluna para armazenar os ts_vectors computados
ALTER TABLE question
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE OR REPLACE FUNCTION question_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = (
            setweight(to_tsvector('english', NEW.title), 'A') ||
            setweight(to_tsvector('english', NEW.content), 'B')
        );
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF (NEW.title <> OLD.title OR NEW.content <> OLD.content) THEN
            NEW.tsvectors = (
                setweight(to_tsvector('english', NEW.title), 'A') ||
                setweight(to_tsvector('english', NEW.content), 'B')
            );
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER question_search_update
    BEFORE INSERT OR UPDATE ON question
    FOR EACH ROW
    EXECUTE PROCEDURE question_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX question_search_idx ON question USING GIN (tsvectors);


-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE OR REPLACE FUNCTION content_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;


-- Adicionar à tabela answer uma coluna para armazenar os ts_vectors computados
ALTER TABLE answer
ADD COLUMN tsvectors TSVECTOR;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER answer_search_update
    BEFORE INSERT OR UPDATE ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE content_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX answer_search_idx ON answer USING GIN (tsvectors);


-- Adicionar à tabela question_comment uma coluna para armazenar os ts_vectors computados
ALTER TABLE question_comment
ADD COLUMN tsvectors TSVECTOR;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER question_comment_search_update
    BEFORE INSERT OR UPDATE ON question_comment
    FOR EACH ROW
    EXECUTE PROCEDURE content_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX question_comment_search_idx ON question_comment USING GIN (tsvectors);


-- Adicionar à tabela answer_comment uma coluna para armazenar os ts_vectors computados
ALTER TABLE answer_comment
ADD COLUMN tsvectors TSVECTOR;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER answer_comment_search_update
    BEFORE INSERT OR UPDATE ON answer_comment
    FOR EACH ROW
    EXECUTE PROCEDURE content_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX answer_comment_search_idx ON answer_comment USING GIN (tsvectors);


-- Gatilhos


-- BR01
CREATE OR REPLACE FUNCTION prevent_self_vote_on_question() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.id_user = (SELECT id_user FROM question WHERE id = NEW.id_question) THEN
        RAISE EXCEPTION 'Cannot vote on your own question';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER prevent_self_vote_on_question
    BEFORE INSERT OR UPDATE ON question_vote
    FOR EACH ROW
    EXECUTE PROCEDURE prevent_self_vote_on_question();


CREATE OR REPLACE FUNCTION prevent_self_vote_on_answer() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.id_user = (SELECT id_user FROM answer WHERE id = NEW.id_answer) THEN
        RAISE EXCEPTION 'Cannot vote on your own answer';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER prevent_self_vote_on_answer
    BEFORE INSERT OR UPDATE ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE prevent_self_vote_on_answer();


-- BR02
CREATE OR REPLACE FUNCTION update_content_on_user_deletion() RETURNS TRIGGER AS
$BODY$
BEGIN
    UPDATE users SET username = CONCAT('anonymous', OLD.id), email = CONCAT('anonymous', OLD.id) WHERE id = OLD.id;
    RETURN NULL;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER update_content_on_user_deletion
    BEFORE DELETE ON users
    FOR EACH ROW
    EXECUTE PROCEDURE update_content_on_user_deletion();


-- BR03
CREATE OR REPLACE FUNCTION calculate_user_rating() RETURNS TRIGGER AS
$BODY$
DECLARE
    total_likes INTEGER;
    total_dislikes INTEGER;
    id_author INTEGER;
    id_c INTEGER;
BEGIN
    SELECT id_user INTO id_author
    FROM answer
    WHERE answer.id = NEW.id_answer;

    SELECT id_community INTO id_c
    FROM answer JOIN question ON answer.id_question = question.id
    WHERE answer.id = NEW.id_answer;

    SELECT COUNT(*) INTO total_likes
    FROM question JOIN answer ON question.id = answer.id_question JOIN answer_vote ON answer_vote.id_answer = answer.id 
    WHERE answer.id_user = id_author AND question.id_community = id_c AND answer_vote.likes = TRUE;

    SELECT COUNT(*) INTO total_dislikes
    FROM question JOIN answer ON question.id = answer.id_question JOIN answer_vote ON answer_vote.id_answer = answer.id 
    WHERE answer.id_user = id_author AND question.id_community = id_c AND answer_vote.likes = FALSE;
    IF total_likes + total_dislikes = 0 THEN
        RETURN NEW;
    END IF;

    IF EXISTS (SELECT id_user FROM reputation WHERE id_user = id_author) THEN
        UPDATE reputation
        SET rating = 1000 * total_likes / (total_likes + total_dislikes)
        WHERE id_user = id_author AND id_community = id_c;
    ELSE
        INSERT INTO reputation (id_user, id_community, rating)
        VALUES (id_author, id_c, 1000 * total_likes / (total_likes + total_dislikes));
    END IF;

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER calculate_user_rating
    AFTER INSERT OR UPDATE ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE calculate_user_rating();


-- BR04
CREATE OR REPLACE FUNCTION check_expert_status() RETURNS TRIGGER AS
$BODY$
DECLARE
    total_badges INTEGER;
    user_badges INTEGER;
BEGIN
    SELECT COUNT(*) INTO total_badges FROM badge;
    SELECT COUNT(*) INTO user_badges FROM user_earns_badge WHERE id_user = NEW.id_user;

    IF total_badges = user_badges AND NEW.rating > 800 THEN
        SET NEW.expert = TRUE;
    ELSE
        SET NEW.expert = FALSE;
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_expert_status
    AFTER INSERT OR UPDATE ON reputation
    FOR EACH ROW
    EXECUTE PROCEDURE check_expert_status();


-- BR05
CREATE OR REPLACE FUNCTION check_file_extension() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF NEW.file NOT LIKE '%.jpg' AND 
       NEW.file NOT LIKE '%.jpeg' AND 
       NEW.file NOT LIKE '%.png' AND 
       NEW.file NOT LIKE '%.txt' AND 
       NEW.file NOT LIKE '%.pdf' AND 
       NEW.file NOT LIKE '%.doc' THEN
       RAISE EXCEPTION 'Invalid file extension for %. Only jpg, jpeg, png, txt, pdf, doc are allowed.', NEW.file;
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_file_extension_on_question
    BEFORE INSERT OR UPDATE ON question
    FOR EACH ROW
    EXECUTE PROCEDURE check_file_extension();

CREATE TRIGGER check_file_extension_on_answer
    BEFORE INSERT OR UPDATE ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE check_file_extension();


-- BR06
CREATE OR REPLACE FUNCTION check_question_vote() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF EXISTS (SELECT id_user FROM question_vote WHERE id_question = NEW.id_question AND id_user = NEW.id_user) THEN
        RAISE EXCEPTION 'Each user can only vote once on each question.';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_question_vote
    BEFORE INSERT OR UPDATE ON question_vote
    FOR EACH ROW
    EXECUTE PROCEDURE check_question_vote();

CREATE OR REPLACE FUNCTION check_answer_vote() RETURNS TRIGGER AS
$BODY$
BEGIN
    IF EXISTS (SELECT id_user FROM answer_vote WHERE id_answer = NEW.id_answer AND id_user = NEW.id_user) THEN
        RAISE EXCEPTION 'Each user can only vote once on each answer.';
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER check_answer_vote
    BEFORE INSERT ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE check_answer_vote();


-- Notificações

CREATE OR REPLACE FUNCTION new_answer_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT id_user, title INTO author, q_title
    FROM question
    WHERE id = NEW.id_question;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new answer to your question: ', q_title, '!'), CURRENT_DATE, FALSE, author);
    
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_answer_notification
    AFTER INSERT ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE new_answer_notification();


CREATE OR REPLACE FUNCTION new_question_vote_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT id_user, title INTO author, q_title
    FROM question
    WHERE id = NEW.id_question;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new vote on your question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_question_vote_notification
    AFTER INSERT ON question_vote
    FOR EACH ROW
    EXECUTE PROCEDURE new_question_vote_notification();


CREATE OR REPLACE FUNCTION new_answer_vote_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT answer.id_user, title INTO author, q_title
    FROM answer JOIN question ON answer.id_question = question.id
    WHERE answer.id = NEW.id_answer;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new vote on your answer to the question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_answer_vote_notification
    AFTER INSERT ON answer_vote
    FOR EACH ROW
    EXECUTE PROCEDURE new_answer_vote_notification();


CREATE OR REPLACE FUNCTION new_question_comment_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT id_user, title INTO author, q_title
    FROM question
    WHERE id = NEW.id_question;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new comment on your question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_question_comment_notification
    AFTER INSERT ON question_comment
    FOR EACH ROW
    EXECUTE PROCEDURE new_question_comment_notification();


CREATE OR REPLACE FUNCTION new_answer_comment_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    author INTEGER;
    q_title VARCHAR(255);
BEGIN
    SELECT answer.id_user, title INTO author, q_title
    FROM answer JOIN question ON answer.id_question = question.id
    WHERE answer.id = NEW.id_answer;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new comment on your answer to the question: ', q_title, '!'), CURRENT_DATE, FALSE, author);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_answer_comment_notification
    AFTER INSERT ON answer_comment
    FOR EACH ROW
    EXECUTE PROCEDURE new_answer_comment_notification();


CREATE OR REPLACE FUNCTION new_badge_notification() RETURNS TRIGGER AS
$BODY$
DECLARE
    b_name VARCHAR(255);
BEGIN
    SELECT name INTO b_name
    FROM badge
    WHERE id = NEW.id_badge;

    INSERT INTO notification (content, date, read, id_user)
    VALUES (CONCAT('You received a new badge: ', b_name, '!'), CURRENT_DATE, FALSE, NEW.id_user);

    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER new_badge_notification
    AFTER INSERT ON user_earns_badge
    FOR EACH ROW
    EXECUTE PROCEDURE new_badge_notification();


-- Emblemas

CREATE OR REPLACE FUNCTION award_badge_on_first_question() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM question WHERE id_user = NEW.id_user) = 1 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 1);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_question
    AFTER INSERT ON question
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_question();

CREATE OR REPLACE FUNCTION award_badge_on_first_answer() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM answer WHERE id_user = NEW.id_user) = 1 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 2);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_answer
    AFTER INSERT ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_answer();

CREATE OR REPLACE FUNCTION award_badge_on_first_comment_question() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM question_comment WHERE id_user = NEW.id_user) = 1 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 3);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_comment_question
    AFTER INSERT ON question_comment
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_comment_question();

CREATE OR REPLACE FUNCTION award_badge_on_first_comment_answer() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM answer_comment WHERE id_user = NEW.id_user) = 1 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 4);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_comment_answer
    AFTER INSERT ON answer_comment
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_comment_answer();

CREATE OR REPLACE FUNCTION award_badge_on_first_100_question() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM question WHERE id_user = NEW.id_user) = 100 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 5);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_100_question
    AFTER INSERT ON question
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_100_question();

CREATE OR REPLACE FUNCTION award_badge_on_first_100_answer() RETURNS TRIGGER AS 
$BODY$
BEGIN
    IF (SELECT COUNT(*) FROM answer WHERE id_user = NEW.id_user) = 100 THEN
        INSERT INTO user_earns_badge (id_user, id_badge) VALUES (NEW.id_user, 6);
    END IF;
    RETURN NEW;
END
$BODY$
LANGUAGE plpgsql;

CREATE TRIGGER award_badge_on_first_100_answer
    AFTER INSERT ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE award_badge_on_first_100_answer();
```


### A.2. Povoamento da Base de Dados

O excerto do *script* de povoamento da base de dados presente abaixo inclui o código necessário para preencher a base de dados com um número razoável de tuplos de valores plausíveis para os campos das tabelas criadas anteriormente.

As tabelas *notification*, *user_earns_badge* e *reputation* não contêm operações de inserção neste ficheiro porque tal é feito automaticamente pelos gatilhos definidos com os dados inseridos nas outras tabelas.

O *script* completo para o povoamento da base de dados encontra-se disponível em LINK.

```sql
TODO
```


---


## Histórico de Revisões

Nada a assinalar.

***
GROUP23114, 22/10/2023

* António Henrique Martins Azevedo, up202108689@up.pt
* António Marujo Rama, up202108801@up.pt (Editor)
* Manuel Ramos Leite Carvalho Neto, up202108744@up.pt
* Matilde Isabel da Silva Simões, up202108782@up.pt