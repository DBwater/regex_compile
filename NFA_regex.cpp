#include<stdio.h>
#include<string.h>
#include<stdlib.h>	
char *getrepost(char *re)
{
	int nalt=0,natom=0;//|的个数,需要natom个.来分开结构
	static char buf[8000];
	char *dft;
	struct {
		int nalt,natom;
	}parent[100],*p;//parent是一个括号里面结构
	p=parent;
	dft=buf;
	if(strlen(re)>sizeof(buf)/2)return NULL;//防止空间开的过小
	for(;*re;re++){
		switch(*re){
			case '(':
				if(natom>1){
					natom--;
					*dft++='.';
				}
				if(p>=parent+100)
					return NULL;//防止指针越界
				p->nalt=nalt;
				p->natom=natom;
				p++;
				nalt=0;natom=0;
				break;
			case '|'://|的优先级最低其他符号优先级相同
				if(natom==0)
					return NULL;
				while(--natom>0)
					*dft++='.';
				nalt++;
				break;
			case ')':
				if(p==parent)return NULL;//判断是否有'('
				if(natom==0)return NULL;//判断括号里面是否有结构
				while(--natom>0)*dft++='.';
				for(;nalt>0;nalt--)*dft++='|';//|的优先级最低，不懂的可以参考中缀转后缀表达式的构建
				--p;//一个括号匹配完毕
				nalt=p->nalt;
				natom=p->natom;
				natom++;//已经匹配完的括号里的算作一个部分用.隔开
				break;
			case '*':
			case '+':
			case '?':
				if(natom==0)return NULL;
				*dft++=*re;
				break;
			default:
				if(natom>1){
					natom--;
					*dft++='.';//每2个字符需要一个.来分割
				}	
				*dft++=*re;
				//printf("%c",*re);
				natom++;
				break;
		}
	}
	if(p!=parent)return NULL;//括号没有匹配完成
	while(--natom>0)*dft++='.';
	for(;nalt>0;nalt--) *dft++='|';
	*dft=0;
	return buf;
}
int nstate;//state个数
typedef struct State{  //节点结构
	int c;
	State *out;
	State *out1;
	int lastlist;
}State;

State *state(int c,State *out,State *out1)
{
	State *s;
	nstate++;
	s=(State*)malloc(sizeof(*s));//注意在c++中malloc返回的是void类型
	s->c=c;
	s->out=out;
	s->out1=out1;
	s->lastlist=0;
	return s;
}
enum{
	Match=256,//结束符号
	Split=257//空的连接State结构
};
State matchstate={Match};//结束状态
typedef struct Ptrlist{
	State *s;
	Ptrlist *next;
}Ptrlist;
Ptrlist *list1(State **outp)
{
	Ptrlist *l;
	l=(Ptrlist*)outp;
	l->next=NULL;
	return l;
}
typedef struct Frag{
	State* start;
	Ptrlist *out;
}Frag;
Frag frag(State *start,Ptrlist *out)
{
	Frag fr={start,out};
	return fr;
}
void patch(Ptrlist *l,State *s)
{
	Ptrlist *next;
	for(;l;l=next){
		next=l->next;
		l->s=s;
	}
}
Ptrlist* append(Ptrlist *l1,Ptrlist *l2)
{
	Ptrlist *l3;
	l3=l1;
	while(l1->next)
		l1=l1->next;
	l1->next=l2;
	return l3;
}
State *getnfa(char *postfix)
{
	char *p;
	Frag stack[1000],*stackp,e1,e2,e;
	State *s;
	if(postfix==NULL)
		return NULL;
	#define push(s) *stackp++=s;
	#define pop() *--stackp;
	stackp=stack;
	for(p=postfix;*p;p++){
		switch(*p){
			case '.':
				e2=pop();
				e1=pop();
				patch(e1.out,e2.start);
				push(frag(e1.start,e2.out));
				break;
			case '|':
				e2=pop();
				e1=pop();
				s=state(Split,e1.start,e2.start);
				push(frag(s,append(e1.out,e2.out)));
				break;
			case '*':
				e=pop();
				s=state(Split,e.start,NULL);
				patch(e.out,s);
				push(frag(s,list1(&s->out1)));
				break;
			case '+':
				e=pop();
				s=state(Split,e.start,NULL);
				patch(e.out,s);
				push(frag(e.start,list1(&s->out1)));
				break;
			case '?':
				e=pop();
				s=state(Split,e.start,NULL);
				push(frag(s,append(e.out,list1(&s->out1))));
				break;
			default:
				s=state(*p,NULL,NULL);
				push(frag(s,list1(&s->out)));
				break;
		}
	}
	e=pop();
	if(stack!=stack) return NULL;
	patch(e.out,&matchstate);//结束状态
	return e.start;
	#undef push
	#undef pop
}
typedef struct List{	//记录所有当前状态
	State **s;
	int n;//记录状态个数
}List;
List l1,l2;
static int listid;//走到该状态需要的步数
void addstate(List *l,State *s)//把状态s加入到list中
{
	if(s==NULL||s->lastlist==listid)//状态为空或者状态重复
		return ;
	s->lastlist=listid;
	if(s->c==Split){//如果当前状态为空的连接符号
		addstate(l,s->out);
		addstate(l,s->out1);
		return ;
	}
	l->s[l->n++]=s;//把当前状态加入到l

}
List* startlist(State *start,List *l)//初始化状态list
{
	l->n=0;
	listid++;
	addstate(l,start);
	return l;
}
bool ismatch(List *l)//判断最后的状态集合中是否有结束状态
{
	int i;
	for(i=0;i<l->n;i++)
		if(l->s[i]==&matchstate)
			return true;
	return false;
}
void step(List *clist,int c,List *nlist)//状态从clist集合转移到nlist集合
{
	State *s;
	listid++;
	nlist->n=0;
	for(int i=0;i<clist->n;i++){
		s=clist->s[i];
		if(s->c==c)
			addstate(nlist,s->out);
	}
}
int match(State *start,char *s)//在NFA上跑字符串
{
	int i,c;
	List *clist,*nlist,*t;
	clist=startlist(start,&l1);
	nlist=&l2;
	for(;*s;s++){
		c= *s & 0xFF;//取后8位
		step(clist,c,nlist);
		t=clist;clist=nlist;nlist=t;
	}
	return ismatch(clist);
}
char tt[10][1000];
int main()
{
	scanf("%s",tt[1]);
	scanf("%s",tt[2]);
	char *post=getrepost(tt[1]);
	if(post==NULL){
		printf("非法输入正则表达式\n");
		return 1;
	}
	State *start=getnfa(post);
	if(start==NULL){
		printf("构建NFA失败\n");
		return 1;
	}
	l1.s=(struct State **)malloc(nstate*sizeof(l1.s[0]));
	l2.s=(struct State **)malloc(nstate*sizeof(l2.s[0]));
	if(match(start,tt[2]))
		printf("匹配成功:%s\n",tt[2]);
	else printf("匹配失败\n");

}
