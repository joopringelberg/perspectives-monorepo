import React, { useEffect, useState } from "react";
import { Alert, Button, Form, Modal } from "react-bootstrap";
import { ContextInstanceT, ContextSearchOption, ContextType, PDRproxy } from "perspectives-proxy";
import { i18next } from "perspectives-react";

interface FindContextProps {
  onClose: () => void;
  onOpen: (context: ContextInstanceT) => void;
  onCopy: (context: ContextInstanceT, contextType: ContextType, title: string) => void;
}

interface OptionsState {
  options: ContextSearchOption[];
  loading: boolean;
  failed: boolean;
}

const emptyOptions: OptionsState = { options: [], loading: false, failed: false };
const loadingOptions: OptionsState = { options: [], loading: true, failed: false };

export default function FindContext({ onClose, onOpen, onCopy }: FindContextProps) {
  const [language, setLanguage] = useState(i18next.language);
  const [models, setModels] = useState<OptionsState>(loadingOptions);
  const [types, setTypes] = useState<OptionsState>(emptyOptions);
  const [contexts, setContexts] = useState<OptionsState>(emptyOptions);
  const [modelId, setModelId] = useState("");
  const [typeId, setTypeId] = useState("");
  const [contextId, setContextId] = useState("");
  const t = (key: string) => i18next.t(key, { ns: "mycontexts" });

  useEffect(() => {
    const changed = (lng: string) => setLanguage(lng);
    i18next.on("languageChanged", changed);
    return () => { i18next.off("languageChanged", changed); };
  }, []);

  useEffect(() => {
    let cancelled = false;
    setModels(loadingOptions);
    PDRproxy.then(proxy => proxy.getContextSearchModels()).then(
      options => { if (!cancelled) setModels({ options, loading: false, failed: false }); },
      () => { if (!cancelled) setModels({ ...emptyOptions, failed: true }); }
    );
    return () => { cancelled = true; };
  }, [language]);

  useEffect(() => {
    let cancelled = false;
    if (!modelId) {
      setTypes(emptyOptions);
      return;
    }
    setTypes(loadingOptions);
    PDRproxy.then(proxy => proxy.getContextSearchTypes(modelId)).then(
      options => { if (!cancelled) setTypes({ options, loading: false, failed: false }); },
      () => { if (!cancelled) setTypes({ ...emptyOptions, failed: true }); }
    );
    return () => { cancelled = true; };
  }, [modelId, language]);

  useEffect(() => {
    let cancelled = false;
    if (!typeId) {
      setContexts(emptyOptions);
      return;
    }
    setContexts(loadingOptions);
    PDRproxy.then(proxy => proxy.getContextSearchInstances(typeId as ContextType)).then(
      options => { if (!cancelled) setContexts({ options, loading: false, failed: false }); },
      () => { if (!cancelled) setContexts({ ...emptyOptions, failed: true }); }
    );
    return () => { cancelled = true; };
  }, [typeId, language]);

  const selectedContext = contexts.options.find(option => option.id === contextId);
  const canAct = !!selectedContext && !models.loading && !types.loading && !contexts.loading
    && !models.failed && !types.failed && !contexts.failed;
  const options = (state: OptionsState, placeholder: string) => <>
    <option value="">{t(state.loading ? "findContext_loading" : placeholder)}</option>
    {[...state.options].sort((a, b) => a.label.localeCompare(b.label, language)).map(option =>
      <option key={option.id} value={option.id}>{option.label}</option>
    )}
  </>;

  return (
    <Modal show onHide={onClose} aria-labelledby="find-context-title">
      <Modal.Header closeButton closeLabel={t("genericClose")}>
        <Modal.Title id="find-context-title">{t("findContext_title")}</Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Form.Group className="mb-3" controlId="find-context-model">
          <Form.Label>{t("findContext_model")}</Form.Label>
          <Form.Select value={modelId} disabled={models.loading || models.failed} onChange={event => {
            setModelId(event.target.value);
            setTypeId("");
            setContextId("");
            setTypes(emptyOptions);
            setContexts(emptyOptions);
          }}>
            {options(models, "findContext_model")}
          </Form.Select>
        </Form.Group>
        <Form.Group className="mb-3" controlId="find-context-type">
          <Form.Label>{t("findContext_type")}</Form.Label>
          <Form.Select value={typeId} disabled={!modelId || types.loading || types.failed} onChange={event => {
            setTypeId(event.target.value);
            setContextId("");
            setContexts(emptyOptions);
          }}>
            {options(types, "findContext_type")}
          </Form.Select>
        </Form.Group>
        <Form.Group className="mb-3" controlId="find-context-instance">
          <Form.Label>{t("findContext_context")}</Form.Label>
          <Form.Select size="sm" htmlSize={6} value={contextId}
            disabled={!typeId || contexts.loading || contexts.failed}
            onChange={event => setContextId(event.target.value)}>
            {options(contexts, "findContext_context")}
          </Form.Select>
        </Form.Group>
        {models.failed || types.failed || contexts.failed ?
          <Alert variant="danger" role="alert">{t("findContext_error")}</Alert> : null}
        {typeId && !contexts.loading && !contexts.failed && contexts.options.length === 0 ?
          <p role="status">{t("findContext_empty")}</p> : null}
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onClose}>{t("findContext_cancel")}</Button>
        <Button disabled={!canAct} onClick={() => {
          if (selectedContext && canAct) {
            onOpen(selectedContext.id as ContextInstanceT);
            onClose();
          }
        }}>{t("findContext_open")}</Button>
        <Button disabled={!canAct} onClick={() => {
          if (selectedContext && canAct) {
            onCopy(selectedContext.id as ContextInstanceT, typeId as ContextType, selectedContext.label);
            onClose();
          }
        }}>{t("findContext_copy")}</Button>
      </Modal.Footer>
    </Modal>
  );
}
