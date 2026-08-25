import { load } from "js-yaml";

const CONTEXT_SCHEMA = "perspectives-context-conversations/v1";
const LIBRARY_SCHEMA = "perspectives-conversation-library/v1";
const RUNTIME_SCHEMA = "perspectives-help/v1";

const splitVersionedModelUri = (uri) => {
  const separator = uri.indexOf("@");
  return separator < 0
    ? { modelUri: uri, version: undefined }
    : { modelUri: uri.slice(0, separator), version: uri.slice(separator + 1) };
};

const validateDocumentModel = (mapping, documentModel, versionedStableModelUri, documentName) => {
  if (typeof documentModel !== "string") fail(documentName, "model must be a versioned readable model URI.");

  const readable = splitVersionedModelUri(documentModel);
  const stable = splitVersionedModelUri(versionedStableModelUri);
  if (stable.modelUri !== mapping.modelIdentifier) {
    fail(documentName, `stable model '${stable.modelUri}' does not match the loaded stable-ID mapping.`);
  }

  const stableModelCuid = stable.modelUri.slice(stable.modelUri.lastIndexOf("#") + 1);
  if (mapping.contextCuids?.[readable.modelUri] !== stableModelCuid) {
    fail(documentName, `readable model '${readable.modelUri}' does not match the loaded stable-ID mapping.`);
  }
  if (readable.version === undefined || readable.version !== stable.version) {
    fail(documentName, `model version must be '${stable.version ?? "<missing>"}'.`);
  }
};

const fail = (documentName, message) => {
  throw new Error(`Conversation source '${documentName}': ${message}`);
};

const assertObject = (value, documentName, description) => {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    fail(documentName, `${description} must be a mapping.`);
  }
  return value;
};

const stableContext = (mapping, readable, documentName) => {
  // The mapping stores local CUIDs. Rebuild the complete stable type URI in the
  // same way as StableIdMapping.idUriForContext, walking enclosing contexts.
  const localCuid = mapping.contextCuids?.[readable];
  if (!localCuid) fail(documentName, `unknown context type '${readable}'.`);
  const separator = readable.lastIndexOf("$");
  if (separator < 0) return mapping.modelIdentifier;
  return `${stableContext(mapping, readable.slice(0, separator), documentName)}$${localCuid}`;
};

const stableRole = (mapping, readable, documentName) => {
  const localCuid = mapping.roleCuids?.[readable];
  if (!localCuid) fail(documentName, `unknown role type '${readable}'.`);
  const separator = readable.lastIndexOf("$");
  if (separator < 0) fail(documentName, `role type '${readable}' has no declaring context.`);
  return `${stableContext(mapping, readable.slice(0, separator), documentName)}$${localCuid}`;
};

const message = (conversationId, path, kind, fallback) => {
  if (typeof fallback !== "string" || fallback.length === 0) {
    throw new Error(`${kind} at ${path.join(".")} must contain non-empty text.`);
  }
  return {
    message: `help.${conversationId}.${kind}.${path.join(".")}`,
    fallback,
  };
};

const normalizeConversation = (body, conversationId, documentName) => {
  const definition = assertObject(body, documentName, `conversation '${conversationId}'`);
  const elements = definition.elements ?? {};
  assertObject(elements, documentName, `elements of '${conversationId}'`);

  const normalizeSequence = (sequence, path, references) => {
    if (!Array.isArray(sequence) || sequence.length === 0) {
      fail(documentName, `sequence at ${path.join(".")} must be a non-empty list.`);
    }

    let openQuestion = false;
    let answerCount = 0;
    const normalized = sequence.flatMap((rawElement, index) => {
      const elementPath = [...path, index + 1];
      const element = assertObject(rawElement, documentName, `element at ${elementPath.join(".")}`);
      const keys = Object.keys(element);
      if (keys.length !== 1) fail(documentName, `element at ${elementPath.join(".")} must have exactly one key.`);

      if (Object.hasOwn(element, "ref")) {
        const label = element.ref;
        if (typeof label !== "string" || !Object.hasOwn(elements, label)) {
          fail(documentName, `unknown element reference '${String(label)}' in '${conversationId}'.`);
        }
        if (references.has(label)) fail(documentName, `cyclic element reference '${label}' in '${conversationId}'.`);
        return normalizeSequence([elements[label]], [...elementPath, label], new Set([...references, label]));
      }

      if (Object.hasOwn(element, "statement")) {
        if (openQuestion && answerCount === 0) fail(documentName, `question before ${elementPath.join(".")} has no answer.`);
        openQuestion = false;
        return [{ statement: message(conversationId, elementPath, "statement", element.statement) }];
      }

      if (Object.hasOwn(element, "question")) {
        if (openQuestion && answerCount === 0) fail(documentName, `question before ${elementPath.join(".")} has no answer.`);
        openQuestion = true;
        answerCount = 0;
        return [{ question: message(conversationId, elementPath, "question", element.question) }];
      }

      if (Object.hasOwn(element, "answer")) {
        if (!openQuestion) fail(documentName, `answer at ${elementPath.join(".")} is not preceded by a question.`);
        answerCount += 1;
        const answer = typeof element.answer === "string" ? { text: element.answer } : assertObject(element.answer, documentName, `answer at ${elementPath.join(".")}`);
        const normalizedAnswer = { answer: message(conversationId, elementPath, "answer", answer.text) };
        if (answer.sequence !== undefined) normalizedAnswer.sequence = normalizeSequence(answer.sequence, [...elementPath, "sequence"], references);
        return [normalizedAnswer];
      }

      if (Object.hasOwn(element, "sequence")) {
        return [{ sequence: normalizeSequence(element.sequence, [...elementPath, "sequence"], references) }];
      }

      fail(documentName, `unsupported element '${keys[0]}' at ${elementPath.join(".")}.`);
    });

    if (openQuestion && answerCount === 0) fail(documentName, `question at the end of ${path.join(".")} has no answer.`);
    return normalized;
  };

  return { conversation: normalizeSequence(definition.conversation, ["conversation"], new Set()) };
};

export const compileConversationSourcesImpl = (documentNames, yamlSources, mapping, versionedModelUri) => {
  if (documentNames.length !== yamlSources.length) throw new Error("Conversation source names and contents are not aligned.");

  const documents = new Map();
  documentNames.forEach((documentName, index) => {
    if (documents.has(documentName)) fail(documentName, "document name is duplicated.");
    const parsed = load(yamlSources[index]);
    const document = assertObject(parsed, documentName, "document");
    if (document.schema !== CONTEXT_SCHEMA && document.schema !== LIBRARY_SCHEMA) {
      fail(documentName, `unsupported schema '${String(document.schema)}'.`);
    }
    validateDocumentModel(mapping, document.model, versionedModelUri, documentName);
    assertObject(document.conversations, documentName, "conversations");
    documents.set(documentName, document);
  });

  const runtime = { schema: RUNTIME_SCHEMA, model: mapping.modelIdentifier, bindings: {}, conversations: {} };
  const runtimeConversationId = (documentName, conversationId) => `${documentName}#${conversationId}`;

  for (const [documentName, document] of documents) {
    for (const [conversationId, body] of Object.entries(document.conversations)) {
      const id = runtimeConversationId(documentName, conversationId);
      runtime.conversations[id] = normalizeConversation(body, id, documentName);
    }
  }

  const resolveReference = (documentName, binding) => {
    const hasLocal = typeof binding.conversation === "string";
    const hasExternal = binding.conversationRef !== undefined;
    if (hasLocal === hasExternal) fail(documentName, "a binding must have exactly one conversation reference.");
    const referenceDocument = hasLocal ? documentName : binding.conversationRef?.document;
    const conversationId = hasLocal ? binding.conversation : binding.conversationRef?.conversation;
    if (!documents.has(referenceDocument)) fail(documentName, `unknown conversation document '${String(referenceDocument)}'.`);
    const id = runtimeConversationId(referenceDocument, conversationId);
    if (!Object.hasOwn(runtime.conversations, id)) fail(documentName, `unknown conversation '${String(conversationId)}'.`);
    return id;
  };

  const putBinding = (container, key, conversationId, documentName) => {
    if (Object.hasOwn(container, key)) fail(documentName, `duplicate help binding for '${key}'.`);
    container[key] = conversationId;
  };

  for (const [documentName, document] of documents) {
    if (document.schema === LIBRARY_SCHEMA) continue;
    const contextType = stableContext(mapping, document.context, documentName);
    const contextBindings = runtime.bindings[contextType] ??= { context: {}, perspectives: {} };

    for (const binding of document.bindings?.context ?? []) {
      if (!Array.isArray(binding.audiences) || binding.audiences.length === 0) fail(documentName, "context binding requires audiences.");
      const conversationId = resolveReference(documentName, binding);
      for (const audience of binding.audiences) {
        putBinding(contextBindings.context, stableRole(mapping, audience, documentName), conversationId, documentName);
      }
    }

    for (const binding of document.bindings?.perspectives ?? []) {
      if (!Array.isArray(binding.audiences) || binding.audiences.length === 0) fail(documentName, "perspective binding requires audiences.");
      const targetRole = stableRole(mapping, binding.targetRole, documentName);
      const conversationId = resolveReference(documentName, binding);
      for (const audience of binding.audiences) {
        const audienceRole = stableRole(mapping, audience, documentName);
        const audienceBindings = contextBindings.perspectives[audienceRole] ??= {};
        const key = binding.perspectiveId === undefined ? targetRole : `${targetRole}#${binding.perspectiveId}`;
        putBinding(audienceBindings, key, conversationId, documentName);
      }
    }
  }

  return JSON.stringify(runtime);
};

export const parseConversationStoreImpl = (json) => {
  const store = JSON.parse(json);
  if (store?.schema !== RUNTIME_SCHEMA || typeof store.bindings !== "object" || typeof store.conversations !== "object") {
    throw new Error("Unsupported or malformed conversations.json attachment.");
  }
  return store;
};

export const resolveConversationImpl = (store, contextType, audienceRoleType, targetRoleType, perspectiveId) => {
  const contextBindings = store.bindings[contextType];
  if (!contextBindings) return null;
  const audienceBindings = contextBindings.perspectives?.[audienceRoleType];
  const conversationId = targetRoleType === ""
    ? contextBindings.context?.[audienceRoleType]
    : perspectiveId === ""
      ? audienceBindings?.[targetRoleType]
      : audienceBindings?.[`${targetRoleType}#${perspectiveId}`] ?? audienceBindings?.[targetRoleType];
  return conversationId === undefined ? null : JSON.stringify(store.conversations[conversationId]);
};
