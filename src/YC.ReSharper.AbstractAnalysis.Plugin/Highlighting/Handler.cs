﻿using System;
using System.Collections.Generic;
using JetBrains.Application.Settings;
using JetBrains.Application.Threading.Tasks;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.CSharp.Errors;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.Tree;
using YC.AbstractAnalysis;
using YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.Dynamic;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    static class Handler
    {
        public static Action<DaemonStageResult> Commiter { get; set; }
        public static IDaemonStageProcess Process { get; set; }
        public static IContextBoundSettingsStore Settings { get; set; }
        private static Helper.ReSharperHelper YcProcessor = Helper.ReSharperHelper.Instance;

        private static ICSharpFile GetCSFile()
        {
            IPsiServices psiServices = Process.DaemonProcess.SourceFile.GetPsiServices();
            return psiServices.Files.GetDominantPsiFile<CSharpLanguage>(Process.DaemonProcess.SourceFile) as ICSharpFile;
        }

        /// <summary>
        /// Do highlighting some tokens chunk.
        /// </summary>
        /// <param name="sender">Now always null</param>
        /// <param name="args"></param>
        private static void OnLexingFinished(object sender, CommonInterfaces.LexingFinishedArgs<ITreeNode> args)
        {
            if (Commiter == null)
                return;

            var consumer = new DefaultHighlightingConsumer(Process, Settings);
            var processor = new TreeNodeProcessor(consumer, GetCSFile());

            string xmlPath = YcProcessor.XmlPath(args.Lang);
            ColorHelper.ParseFile(xmlPath, args.Lang);

            Action action =
                () => args.Tokens.ForEach(node => processor.ProcessAfterInterior(node));

            using (TaskBarrier fibers = Process.DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(action);
            }

            Commiter(new DaemonStageResult(consumer.Highlightings));
        }

        private static void SubscribeYc()
        {
            foreach (var e in YcProcessor.LexingFinished)
                e.AddHandler(OnLexingFinished);

            foreach (var e in YcProcessor.ParsingFinished)
                e.AddHandler(OnParsingFinished);
        }

        /// <summary>
        /// Do translate sppf to ReSharper trees and store result. It is need further.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args">Now it contains only language</param>
        private static Dictionary<string, int> parsedSppf = new Dictionary<string, int>();
        private static void OnParsingFinished(object sender, CommonInterfaces.ParsingFinishedArgs args)
        {
            string lang = args.Lang;

            if (!parsedSppf.ContainsKey(lang))
                parsedSppf.Add(lang, 0);
            else
                parsedSppf[lang]++;

            Action action =
                () =>
                {
                    var isEnd = false;
                    while (!isEnd)
                    {
                        Tuple<ITreeNode, bool> res = YcProcessor.GetNextTree(lang, parsedSppf[lang]);
                        ExistingTreeNodes.AddTree(res.Item1);
                        isEnd = res.Item2;
                    }
                };

            using (TaskBarrier fibers = Process.DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(action);
            }
        }
    }
}
