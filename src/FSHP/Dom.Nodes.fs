namespace FSHP.Dom.Nodes

open Microsoft.FSharp.Control
open FSHP.Helper
open FSHP.Dom.Events
open System

exception DomException of int * string

module internal DomExns =
    let indexSizeErr() = (1, "IndexSizeErr")
    let domstringSizeErr() = (2, "DomstringSizeErr")
    let hierarchyRequestErr() = (3, "HierarchyRequestErr")
    let wrongDocumentErr() = (4, "WrongDocumentErr")
    let invalidCharacterErr() = (5, "InvalidCharacterErr")
    let noDataAllowedErr() = (6, "NoDataAllowedErr")
    let noModificationAllowedErr() = (7, "NoModificationAllowedErr")
    let notFoundErr() = (8, "NotFoundErr")
    let notSupportedErr() = (9, "NotSupportedErr")
    let inuseAttributeErr() = (10, "InuseAttributeErr")
    let invalidStateErr() = (11, "InvalidStateErr")
    let syntaxErr() = (12, "SyntaxErr")
    let invalidModificationErr() = (13, "InvalidModificationErr")
    let namespaceErr() = (14, "NamespaceErr")
    let invalidAccessErr() = (15, "InvalidAccessErr")
    let validationErr() = (16, "ValidationErr")
    let typeMismatchErr() = (17, "TypeMismatchErr")
    let securityErr() = (18, "SecurityErr")
    let networkErr() = (19, "NetworkErr")
    let abortErr() = (20, "AbortErr")
    let urlMismatchErr() = (21, "UrlMismatchErr")
    let quotaExceededErr() = (22, "QuotaExceededErr")
    let timeoutErr() = (23, "TimeoutErr")
    let invalidNodeTypeErr() = (24, "InvalidNodeTypeErr")
    let dataCloneErr() = (25, "DataCloneErr")

type NodeType =
    | Element = 1
    | Attribute = 2
    | Text = 3
    | CDataSection = 4
    | EntityReference = 5
    | Entity = 6
    | ProcessingInstruction = 7
    | Comment = 8
    | Document = 9
    | DocumentType = 10
    | DocumentFragment = 11
    | Notation = 12

type DocumentPosition =
    | Disconnected = 0x01
    | Preceding = 0x02
    | Following = 0x04
    | Contains = 0x08
    | ContainedBy = 0x10
    | ImplementationSpecific = 0x20

// not implementing the baseURI attribute
type Node(nType : NodeType, ?name : string) =
    let name = defaultArg name ""
    let mutable children = List.empty<Node>
    let mutable nodeList = new NodeList(children)
    let mutable doc = Unchecked.defaultof<Document>
    let mutable parent = Unchecked.defaultof<Node>
    member this.NodeType = int nType
    member this.NodeName = name
    member this.OwnerDocument
        with get() = doc and internal set value = doc <- value
    member this.ParentNode 
        with get() = parent and internal set value = parent <- value
    member this.ParentElement 
        with get() = if parent.NodeType = 1 then parent else Unchecked.defaultof<Node>
    member this.HasChildNodes() = List.isEmpty children |> not
    member internal this.NodeChildren 
        with get() = children
        and set value = children <- value; nodeList <- new NodeList(children)
    member this.ChildNodes = nodeList
    static member internal GetFirstChild items = listFirst Unchecked.defaultof<Node> items
    member this.FirstChild with get() = Node.GetFirstChild children
    member this.LastChild = children |> List.rev |> Node.GetFirstChild
    static member internal GetPrevSibling (node:Node) =
        let children = node.ParentNode.NodeChildren
        let rec loop nodes =
            match nodes with
            | [] -> Unchecked.defaultof<Node>
            | a :: b :: _ when node.IsSameNode b -> a
            | _ :: tl -> loop tl
        loop children
    static member internal GetNextSibling (node:Node) =
        let children = node.ParentNode.NodeChildren
        let rec loop nodes =
            match nodes with
            | [] -> Unchecked.defaultof<Node>
            | a :: b :: _ when node.IsSameNode a -> b
            | _ :: tl -> loop tl
        loop children
    member this.PreviousSibling = Node.GetPrevSibling this
    member this.NextSibling = Node.GetNextSibling this
    //member this.CompareDocumentPosition node = 
    // TODO: implement this
    member this.NodeValue =
        match this.NodeType with
        | 3 | 8 | 7 -> this.TextContent
        | _ -> None
    member this.TextContent with get() = None
    member internal this.IsAncestorOf node =
        let rec loop (n:Node) =
            match n.ParentNode with
            | p when p = Unchecked.defaultof<Node> -> false
            | p when this.IsSameNode p -> true
            | p -> loop p.ParentNode
        loop node
    member internal this.IsOfTypes types =
        List.exists (fun t -> t = this.NodeType) types
    member internal this.HasChildOf types =
        List.exists (fun (child : Node) -> child.IsOfTypes types) children
    member internal this.GetChildrenByType types =
        List.filter (fun (child : Node) -> child.IsOfTypes types) children
    member internal this.HasChild node =
        List.exists (fun (child : Node) -> child.IsSameNode node) children
    static member internal RaiseIfNotOf types (node : Node) =
        if not (node.IsOfTypes types) then raise (DomException(DomExns.hierarchyRequestErr()))
    static member internal RaiseIfSelfOrAncestor target (node : Node) =
        if node.IsSameNode target || node.IsAncestorOf target 
            then raise (DomException(DomExns.hierarchyRequestErr()))
    static member internal RaiseIfDocTypeAndHasOwner (node : Node) =
        if node.NodeType = 10 && not (node.OwnerDocument = Unchecked.defaultof<Document>)
            then raise (DomException(DomExns.notSupportedErr()))
    static member internal RaiseIfNotChild child (node : Node) =
        if not (node.HasChild child) then raise (DomException(DomExns.notFoundErr()))
    member this.InsertBefore (newChild : Node, refChild) =
        do Node.RaiseIfNotOf [9;11;1] this
        match refChild with
        | nil when nil = Unchecked.defaultof<Node> -> this.AppendChild newChild
        | _ when not (this.HasChild refChild) ->
            raise (DomException(DomExns.notFoundErr()))
        | _ when this.IsSameNode refChild || refChild.IsAncestorOf this ->
            raise (DomException(DomExns.hierarchyRequestErr()))
        | _ when refChild.NodeType = 10 && refChild.OwnerDocument <> Unchecked.defaultof<Document> ->
            raise (DomException(DomExns.notSupportedErr()))
        | _ ->
            let newChild1 = if newChild.NodeType = 10 
                                then newChild.OwnerDocument <- doc; newChild
                                else this.OwnerDocument.AdpotNode newChild 
            // TODO: again we got to handle the DocumentFragment type
            do this.NodeChildren <- insertBefore newChild1 refChild (===) children
            newChild1
    member this.ReplaceChild(newChild, oldChild) =
        do Node.RaiseIfNotOf [9;11;1] this
        do Node.RaiseIfNotChild oldChild this
        do Node.RaiseIfSelfOrAncestor this newChild
        do Node.RaiseIfDocTypeAndHasOwner newChild
        let newChild1 = if newChild.NodeType = 10 
                            then newChild.OwnerDocument <- doc; newChild
                            else this.OwnerDocument.AdpotNode newChild
        do this.NodeChildren <- removeItem oldChild (===) children
        this.InsertBefore(newChild1, oldChild.NextSibling)
    member this.RemoveChild (oldChild : Node) =
        do Node.RaiseIfNotOf [9;11;1] this
        do Node.RaiseIfNotChild oldChild this
        do this.NodeChildren <- removeItem oldChild (===) children
        oldChild
    member this.AppendChild newChild =
        do Node.RaiseIfNotOf [9;11;1] this
        do Node.RaiseIfSelfOrAncestor newChild this
        do Node.RaiseIfDocTypeAndHasOwner newChild
        let newChild1 = if newChild.NodeType = 10 
                            then newChild.OwnerDocument <- doc; newChild
                            else this.OwnerDocument.AdpotNode newChild
        // TODO: append DocumentFragment in the depth-first way
        do this.NodeChildren <- newChild1 :: children |> List.rev
        newChild1
    // XXX: not implementing CloneNode
    member this.IsSameNode node = this === node
    // XXX: not implementing IsEqualNode, LookUpPrefix, LookNamespaceURI, IsDefaultNamespace


and NodeList (items : Node list) =
    member this.Item with get(idx) = List.nth items idx
    member this.Length = List.length items

and Document (url : string, charset : string, mediaType : string) as this =
    inherit Node(NodeType.Document)
    let url = url
    let mutable charset = charset
    let mediaType = mediaType
    do this.OwnerDocument <- this
    new() = Document("about:blank", "UTF-8", "application/xml")
    member this.URL = url
    member this.DocumentURL = url
    member this.CompatMode = "CSS1Compat"   // no quirks mode supported in our implementation
    //member this.Implementation =
    member this.Charset 
        with get() = charset
        and set value = charset <- value
    member this.CharacterSet = charset
    member this.DefaultCharset = "UTF-8"
    member this.ContentType = mediaType
    member this.DocType = List.find (fun (n:Node) -> n.NodeType = 10) this.NodeChildren
    member this.DocumentElement = 
        this.NodeChildren
            |> List.find (fun (n:Node) -> n.NodeType = 1) 
            |> fun n -> n :?> Element
    member this.GetElementByTagName localName =
        let rec kFold (node:Node) =
            node :: (List.fold (fun acc n -> (kFold n) @ acc) [] node.NodeChildren)
        let filterFun = fun (n:Node) -> n.NodeType = 1 &&
                                        match localName with
                                        | "*" -> true
                                        | _ -> n.NodeName = localName
        new NodeList(List.filter filterFun (kFold this))
    member this.GetElementsByClassName names =
        let refClasses = (regex "\s+").Split(names) |> List.ofArray
        let rec kFold (node:Node) =
            node :: (List.fold (fun acc n -> (kFold n) @ acc) [] node.NodeChildren)
        kFold this 
            |> List.filter (fun (n:Node) -> n.NodeType = 1 )
            |> List.filter (fun n -> 
               List.forall (fun rc -> 
               List.exists (fun c -> c = rc) (n :?> Element).ClassList) refClasses)
    member this.GetElementById (elemId:string) =
    // FIXME: ugly enough
        let rec loop (elem:Element) k =
            match elem.Id with
            | idVal when idVal = elemId -> k elem
            | _ -> 
                let rec loopInner elems = 
                    match elems with
                    | [] -> k Unchecked.defaultof<Element>
                    | hd :: tl -> 
                        loop hd (fun n -> 
                        match n with 
                        | nil when nil = Unchecked.defaultof<Element> -> loopInner tl 
                        | targetElem -> k targetElem)
                loopInner elem.ElemChildren
        loop this.DocumentElement id
    member this.CreateElement name = 
        let elem = new Element(name)
        elem.OwnerDocument <- this
        elem
    // XXX: CreateElementNS, CreateDocumentFragment
    // and CreateProcessingInstruction are not supported
    member this.CreateTextNode data = 
        let elem = new Text(data)
        elem.OwnerDocument <- this
        elem
    member this.CreateComment data =
        let elem = new Comment(data)
        elem.OwnerDocument <- this
        elem
    member this.AdpotNode (node:Node) =
        match node.NodeType with
        | 9 | 10 -> raise (DomException(DomExns.notSupportedErr()))
        | _ -> 
            if not (node.ParentNode = Unchecked.defaultof<Node>) 
                then node.ParentNode.RemoveChild(node) |> ignore
            let rec loop (item:Node) =
                item.OwnerDocument <- this
                List.map (fun (n:Node) -> n.OwnerDocument <- this) item.NodeChildren |> ignore
            loop node
            node

and Element(name : string, ns : string, prefix : string) =
    inherit Node(NodeType.Element, name)
    let ns = ns
    let prefix = prefix
    let mutable attrs = List.empty<Attr>
    let mutable id = ""
    let mutable classList = List.empty<string>
    let mutable classes = classList |> List.toSeq
    new(name) = Element(name, "", "")
    member this.NamespaceURI = ns
    member this.Prefix = prefix
    member this.LocalName = this.NodeName
    member this.TagName = 
        if prefix <> "" 
            then prefix + ":" + this.LocalName 
            else this.LocalName
    member this.Id with get() = id and set value = id <- value
    member internal this.ClassList = classList
    member this.Classes = classes
    member this.AddClass name = 
        if List.forall (fun cls -> cls <> name) classList
            then classList <- name :: classList
                 this.SetClass()
    member this.RemoveClass name =
        classList <- removeItem name (=) classList
        this.SetClass()
    member internal this.SetClass() =
        classes <- classList |> List.toSeq
        let newClass = List.fold (fun acc s -> acc + " " + s) "" classList
        this.SetAttribute("class", newClass.Trim())
    member this.Attributes = attrs |> List.toSeq
    member this.GetAttribute name =
        (List.find (fun (attr:Attr) -> attr.Name = name) attrs).Value
    // XXX: not implementing the GetAttributeNS
    member this.SetAttribute(name, value) =
        let attr = (List.find (fun (attr:Attr) -> attr.Name = name) attrs)
        attr.Value <- value
    // XXX: not implementing the SetAttributeNS
    member this.RemoveAttribute name =
        removeItem name (fun (attr:Attr) n -> attr.Name = n) attrs
    // XXX: not implementing the RemoveAttributeNS
    member this.HasAttribute name =
        List.exists (fun (attr:Attr) -> attr.Name = name) attrs
    member this.GetElementsByTagName name =
    // TODO: make this tail-recursive, actually I want all these to be tail-recursive
        let rec kFold (node:Node) =
            node :: (List.fold (fun acc n -> (kFold n) @ acc) [] node.NodeChildren)
        let filterFun = fun (n:Node) -> n.NodeType = 1 &&
                                        match name with
                                        | "*" -> true
                                        | _ -> n.NodeName = name
        new NodeList(List.filter filterFun (kFold this))
    member this.GetElementsByClassName names =
        let refClasses = (regex "\s+").Split(names) |> List.ofArray
        let rec kFold (node:Node) =
            node :: (List.fold (fun acc n -> (kFold n) @ acc) [] node.NodeChildren)
        kFold this 
            |> List.filter (fun (n:Node) -> n.NodeType = 1 )
            |> List.filter (fun n -> 
               List.forall (fun rc -> 
               List.exists (fun c -> c = rc) (n :?> Element).ClassList) refClasses)
    member internal this.ElemChildren =
        this.NodeChildren 
            |> List.filter (fun n -> n.NodeType = 1)
            |> List.map (fun n -> n :?> Element)
    member this.Children = new HTMLCollection(this.ElemChildren)
    member this.FirstElementChild =
        this.NodeChildren 
            |> List.find (fun (n:Node) -> n.NodeType = 1) 
            |> fun n -> n :?> Element
    member this.LastElementChild =
        this.NodeChildren 
            |> List.rev
            |> List.find (fun (n:Node) -> n.NodeType = 1) 
            |> fun n -> n :?> Element
    member this.PreviousElementSibling =
        let children = this.ParentNode.NodeChildren
        let rec loop nodes acc =
            match nodes with
            | [] -> Unchecked.defaultof<Node>
            | hd :: tl when this.IsSameNode hd -> 
                match acc with
                | [] -> Unchecked.defaultof<Node>
                | a :: _ -> a
            | hd :: tl when hd.NodeType = 1 ->
                loop tl (hd :: acc)
            | _ :: tl -> loop tl acc
        loop children [] |> fun n -> n :?> Element
    member this.NextElementSibling =
        let children = this.ParentNode.NodeChildren
        let rec loop nodes =
            match nodes with
            | [] -> Unchecked.defaultof<Node>
            | hd :: tl when this.IsSameNode hd -> 
                List.find (fun n -> n.NodeType = 1) tl
            | _ :: tl -> loop tl
        loop children |> fun n -> n :?> Element
    member this.ChildElementCount =
        this.NodeChildren 
            |> List.filter (fun n -> n.NodeType = 1) 
            |> List.length

and CharacterData(data : string, cType : NodeType) =
    inherit Node(cType)
    let mutable data = data
    let mutable len = data.Length
    member this.Data 
        with get() = data 
        and set value = data <- value; len <- data.Length
    member this.Length = len
    member this.SubstringData(offset, count) = data.[offset .. offset + count]
    member this.AppendData newData = 
        data <- data + newData
    member this.InsertData(offset, newData) =
        data <- data.[0 .. offset] + newData + data.[offset + 1 ..]
    member this.DeleteData(offset, count) = 
        data <- data.[0 .. offset] + data.[offset + count + 1 ..]
    member this.ReplaceData(offset, count, newData) = 
        this.DeleteData(offset, count); this.InsertData(offset, newData)

and Text(data:string) =
    inherit CharacterData(data, NodeType.Text)
    member this.SplitText offset =
        if offset > this.Length 
            then raise (DomException(DomExns.indexSizeErr()))
            else
                this.SubstringData(offset, this.Length) |> ignore
                this.DeleteData(offset, this.Length)
                let newNode = new Text("")
                newNode.OwnerDocument <- this.OwnerDocument
                newNode.Data <- this.Data
                if this.ParentNode = Unchecked.defaultof<Node>
                    then newNode
                    else 
                        newNode.ParentNode <- this.ParentNode
                        this.ParentNode.InsertBefore(newNode, this.NextSibling) |> ignore
                        newNode
    member this.WholeText =
        let children = this.ParentNode.NodeChildren
        let rec loop found (items:Node list) acc =
            match items with
            | [] -> List.rev acc
            | hd :: tl when hd.NodeType = 3 && this.IsSameNode hd ->
                loop true tl (hd :: acc)
            | hd :: tl when hd.NodeType = 3 ->
                loop found tl (hd :: acc) 
            | _ :: _ when found -> List.rev acc
            | _ :: tl -> 
                loop found tl [] 
        let whole = loop false children [] |> List.map (fun n -> n :?> Text)
        List.fold (fun acc (n:Text) -> acc + n.Data) "" whole
    // TODO: implement replaceWholeText

and Comment(data:string) =
    inherit CharacterData(data, NodeType.Comment)

and Attr(name : string, ?ns : string, ?prefix : string) =
    let ns = defaultArg ns ""
    let prefix = defaultArg prefix ""
    let mutable value = ""
    member this.Name = name
    member this.LocalName = name
    member this.Value
        with get() = value
        and set v = value <- v

and HTMLCollection (items : Element list) =
    member this.Item with get(idx) = List.nth items idx
    member this.Length = List.length items
    member this.NamedItem(key) =
        let tags = ["a"; "applet"; "area"; "embed"; "form"; 
                     "frame"; "frameset"; "iframe"; "img"; "object"]
        items |> List.find (fun i -> 
                 List.exists (fun tag -> 
                 i.TagName = tag && i.GetAttribute "name" = key) tags || 
                 i.Id = key)

and DocumentType (name : string, pid : string, sid : string) =
    member this.Name = name
    member this.PublicId = pid
    member this.SystemId = sid
