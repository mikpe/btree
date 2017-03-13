{Except for source code formatting, this is a verbatim copy of:
 Niklaus Wirth, "Algorithms + Data Structures = Programs", Program 4.7,
 pp 252--257, Prentice-Hall, 1976.  Compiled and tested with GNU Pascal.}
program Btree(input, output);
{B-tree search, insertion and deletion}

const
   n     = 2;
   nn    = 4;  {page size}

type
   ref  = ^page;
   item = record
             key   : integer;
             p     : ref;
             count : integer;
          end;
   page = record
             m  : 0..nn;  {no. of items}
             p0 : ref;
             e  : array [1..nn] of item;
          end;

var
   root, q : ref;
   x       : integer;
   h       : boolean;
   u       : item;

procedure search(x : integer; a : ref; var h : boolean; var v : item);
{Search key x on B-tree with root a; if found, increment counter,
 otherwise insert an item with key x and count 1 in tree. If an item
 emerges to be passed to a lower level, then assign it to v;
 h := "tree a has become higher"}
var k, l, r : integer; q : ref; u : item;
   procedure insert;
   var i : integer; b : ref;
   begin {insert u to the right of a^.e[r]}
      with a^ do
      begin
         if m < nn then
            begin
               m := m+1;
               h := false;
               for i := m downto r+2 do e[i] := e[i-1];
               e[r+1] := u
            end
         else
            begin
               {page a^ is full; split it and assign the emerging item to v}
               new(b);
               if r <= n then
                  begin
                     if r = n then v := u
                     else
                        begin
                           v := e[n];
                           for i := n downto r+2 do e[i] := e[i-1];
                           e[r+1] := u
                        end;
                     for i := 1 to n do b^.e[i] := a^.e[i+n]
                  end
               else
                  begin
                     {insert u in right page}
                     r := r-n;
                     v := e[n+1];
                     for i := 1 to r-1 do b^.e[i] := a^.e[i+n+1];
                     b^.e[r] := u;
                     for i := r+1 to n do b^.e[i] := a^.e[i+n]
                  end;
               m := n;
               b^.m := n;
               b^.p0 := v.p;
               v.p := b
            end
         end {with}
   end {insert};
begin
   {search key x on page a^; h = false}
   if a = nil then
      begin
         {item with key x is not in tree}
         h := true;
         with v do
            begin
               key := x;
               count := 1;
               p := nil
            end
      end
   else
      with a^ do
         begin
            l := 1;
            r := m;
            {binary array search}
            repeat
               k := (l+r) div 2;
               if x <= e[k].key then r := k-1;
               if x >= e[k].key then l := k+1;
            until r < l;
            if l-r > 1 then
               begin
                  {found}
                  e[k].count := e[k].count+1;
                  h := false
               end
            else
               begin
                  {item is not on this page}
                  if r = 0 then q := p0 else q := e[r].p;
                  search(x, q, h, u);
                  if h then insert
               end
         end
end {search};

procedure delete(x : integer; a : ref; var h : boolean);
{search and delete key x in b-tree a; if a page underflow is
 necessary, balance with adjacent page if possible, otherwise merge;
 h := "page a is undersize"}
var i, k, l, r : integer; q : ref;
   procedure underflow(c, a : ref; s : integer; var h : boolean);
   {a = underflow page, c = ancestor page}
   var b : ref; i, k, mb, mc : integer;
   begin
      mc := c^.m;
      {h = true, a^.m = n-1}
      if s < mc then
         begin
            {b := page to the right of a}
            s := s+1;
            b := c^.e[s].p;
            mb := b^.m;
            k := (mb-n+1) div 2;
            {k = no. of items available on adjacent page b}
            a^.e[n] := c^.e[s];
            a^.e[n].p := b^.p0;
            if k > 0 then
               begin
                  {move k items from b to a}
                  for i := 1 to k-1 do a^.e[i+n] := b^.e[i];
                  c^.e[s] := b^.e[k];
                  c^.e[s].p := b;
                  b^.p0 := b^.e[k].p;
                  mb := mb-k;
                  for i := 1 to mb do b^.e[i] := b^.e[i+k];
                  b^.m := mb;
                  a^.m := n-1+k;
                  h := false
               end
            else
               begin
                  {merge pages a and b}
                  for i := 1 to n do a^.e[i+n] := b^.e[i];
                  for i := s to mc-1 do c^.e[i] := c^.e[i+1];
                  a^.m := nn;
                  c^.m := mc-1;
                  {dispose(b)}
                  h := c^.m < n
               end
         end
      else
         begin
            {b := page to the left of a}
            if s = 1 then b := c^.p0
            else b := c^.e[s-1].p;
            mb := b^.m + 1;
            k := (mb-n) div 2;
            if k > 0 then
               begin
                  {move k items from page b to a}
                  for i := n-1 downto 1 do a^.e[i+k] := a^.e[i];
                  a^.e[k] := c^.e[s];
                  a^.e[k].p := a^.p0;
                  mb := mb-k;
                  for i := k-1 downto 1 do a^.e[i] := b^.e[i+mb];
                  a^.p0 := b^.e[mb].p;
                  c^.e[s] := b^.e[mb];
                  c^.e[s].p := a;
                  b^.m := mb-1;
                  a^.m := n-1+k;
                  h := false
               end
            else
               begin
                  {merge pages a and b}
                  b^.e[mb] := c^.e[s];
                  b^.e[mb].p := a^.p0;
                  for i := 1 to n-1 do b^.e[i+mb] := a^.e[i];
                  b^.m := nn;
                  c^.m := mc-1;
                  {dispose(a)}
                  h := c^.m < n
               end
         end
   end {underflow};

   procedure del(p : ref; var h : boolean);
   var q :  ref; {global a,k}
   begin
      with p^ do
         begin
            q := e[m].p;
            if q <> nil then
               begin
                  del(q, h);
                  if h then underflow(p, q, m, h)
               end
            else
               begin
                  p^.e[m].p := a^.e[k].p;
                  a^.e[k] := p^.e[m];
                  m := m-1;
                  h := m < n
               end
         end
   end {del};

begin {delete}
   if a = nil then
      begin
         writeln('KEY IS NOT IN TREE');
         h := false
      end
   else
      with a^ do
         begin
            l := 1;
            r := m;
            {binary array search}
            repeat
               k := (l+r) div 2;
               if x <= e[k].key then r := k-1;
               if x >= e[k].key then l := k+1;
            until l > r;
            if r = 0 then q := p0 else q := e[r].p;
            if l-r > 1 then
               begin
                  {found, now delete e[k]}
                  if q = nil then
                     begin
                        {a is a terminal page}
                        m := m-1;
                        h := m < n;
                        for i := k to m do e[i] := e[i+1];
                     end
                  else
                     begin
                        del(q, h);
                        if h then underflow(a, q, r, h)
                     end
               end
            else
               begin
                  delete(x, q, h);
                  if h then underflow(a, q, r, h)
               end
         end
end {delete};

procedure printtree(p : ref; l : integer);
var i : integer;
begin
   if p <> nil then
      with p^ do
         begin
            for i := 1 to l do write('    ');
            for i := 1 to m do write(e[i].key : 4);
            writeln;
            printtree(p0, l+1);
            for i := 1 to m do printtree(e[i].p, l+1)
         end
end;

begin
   root := nil;
   read(x);
   while x <> 0 do
      begin
         writeln('SEARCH KEY ', x);
         search(x, root, h, u);
         if h then
            begin
               {insert new base page}
               q := root;
               new(root);
               with root^ do
                  begin
                     m := 1;
                     p0 := q;
                     e[1] := u
                  end
            end;
         printtree(root, 1);
         read(x)
      end;
   read(x);
   while x <> 0 do
      begin
         writeln('DELETE KEY ', x);
         delete(x, root, h);
         if h then
            begin
               {base page size was reduced}
               if root^.m = 0 then
                  begin
                     q := root;
                     root := q^.p0;
                     {dispose(q)}
                  end
            end;
         printtree(root, 1);
         read(x)
      end
end.
